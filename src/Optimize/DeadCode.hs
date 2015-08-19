module Optimize.DeadCode where

import AST.Expression.General
import qualified AST.Variable as Var
import qualified AST.Pattern as Pat
import qualified AST.Module as Module
import qualified AST.Expression.Canonical as Can
import qualified AST.Expression.General as Gen
import qualified AST.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import qualified Reporting.Warning as Warning
import qualified Reporting.Result as Result
import qualified Reporting.Error as Error

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Graph as G
import qualified Data.Tree as Tree

import qualified Control.Monad.State as State
import Control.Monad (forM, forM_)
import Control.Applicative ((<$>), (<*>), pure )


-------------------------------------
-- AST with augmented expressions
-- We add a unique identifier to every expression and pattern
-------------------------------------

data ExprFacts =
  ExprFacts
  { exprRegion :: R.Region
  , exprIdent :: Int
  } deriving (Eq, Ord, Show)


type DCEExpr = Gen.Expr ExprFacts Def Var.Canonical Type.Canonical


type DCEExpr' = Gen.Expr' ExprFacts Def Var.Canonical Type.Canonical


type DCEPat = Pat.Pattern ExprFacts Var.Canonical


data Def
    = Def DCEPat DCEExpr (Maybe (A.Located Type.Canonical))
    deriving (Show)


-------------------------------------
-- AST traversals
-- Give each expression a unique integer
-- for easy comparison in the Ref graph
-------------------------------------

addDefIdent :: Can.Def -> State.State Int Def
addDefIdent (Can.Definition pat expr tp) =
  do  nextInt <- State.get
      State.put (nextInt + 1)
      Def <$> addPatIdent pat <*> addExprIdent expr <*> pure tp


addPortIdent
  :: PortImpl Can.Expr t
  -> State.State Int (PortImpl DCEExpr t)
addPortIdent portImpl =
    case portImpl of 
      In p1 p2 ->
        return $ In p1 p2
      
      Out p1 p2 p3 -> do
        newExpr <- addExprIdent p2
        return $ Out p1 newExpr p3
      
      Task p1 p2 p3 -> do
        newExpr <- addExprIdent p2
        return $ Task p1 newExpr p3


addPatIdent
  :: Pat.CanonicalPattern
  -> State.State Int DCEPat
addPatIdent (A.A reg pat) =
  do  innerPat <-
        case pat of
          Pat.Data ctor args ->
            Pat.Data ctor <$> forM args addPatIdent

          Pat.Record p ->
            return $ Pat.Record p

          Pat.Alias nm sub ->
            Pat.Alias nm <$> addPatIdent sub

          Pat.Var p ->
            return $ Pat.Var p

          Pat.Anything ->
            return Pat.Anything

          Pat.Literal p ->
            return $ Pat.Literal p

      nextInt <- State.get
      State.put (nextInt + 1)
      return $ A.A (ExprFacts reg nextInt) innerPat


addExprIdent :: Can.Expr -> State.State Int DCEExpr
addExprIdent (A.A reg e) =
  do let self = addExprIdent

         newExprM =
           case e of
             Literal l ->
               return $ Literal l
         
             Var v ->
               return $ Var v
         
             Range start end ->
               Range <$> self start <*> self end
         
             ExplicitList subList ->
               ExplicitList <$> forM subList self
         
             Binop op arg1 arg2 ->
               (Binop op) <$> self arg1 <*> self arg2
         
             Lambda pat body -> 
               do  newPat <- addPatIdent pat
                   Lambda newPat <$> self body
         
             App fn arg ->
               App <$> self fn <*> self arg
         
             MultiIf pairs final ->
               do let (conds, exprs) = unzip pairs
                  newConds <- forM conds self
                  newExprs <- forM exprs self
                  newFinal <- self final
                  return $ MultiIf (zip newConds newExprs) newFinal
           
             Let defs body ->
               do newDefs <- forM defs addDefIdent
                  newBody <- self body
                  return $ Let newDefs newBody
           
             Case cexp branches ->
               do  let (pats, branchExps) = unzip branches
                   newPats <- forM pats addPatIdent
                   newCexp <- self cexp
                   newExps <- forM branchExps self
                   return $ Case newCexp $ zip newPats newExps
           
             Data ctor args ->
               (Data ctor) <$> forM args self
         
             Access recExp field ->
               Access <$> self recExp <*> return field
         
             Remove recExp field ->
               Remove <$> self recExp <*> return field
         
             Insert recExp field arg ->
               Insert <$> self recExp <*> return field <*> self arg
         
             Modify recExp fieldPairs ->
               do let (names, vals) = unzip fieldPairs
                  newVals <- forM vals self
                  newRec <- self recExp
                  return $ Modify newRec $ zip names newVals
           
             Record pairs ->
               do let (names, vals) = unzip pairs
                  newVals <- forM vals self
                  return $ Record $ zip names newVals
           
             Port sub ->
               Port <$> addPortIdent sub
         
             GLShader a b c ->
               return $ GLShader a b c

             Crash x ->
               return $ Crash x
         
     newExpr <- newExprM
     nextInt <- State.get
     State.put (nextInt + 1)
     return $ A.A (ExprFacts reg nextInt) newExpr

---------------------------------
-- Reference graph data types
---------------------------------

data RefNode =
    ExternalVar Var.Canonical
  | InternalVar Var.Canonical (Int, R.Region)
  | ExprNode Int
  | DefNode Int
  | InitialNode
    deriving (Show, Eq, Ord)


type RefEdge = (RefNode, RefNode)


type RefEnv = Map.Map Var.Canonical (Int, R.Region)


type RefGraph = Map.Map RefNode (Set.Set RefNode)
  

-- Given a pattern, return all the variables (and facts about them)
-- defined by that pattern
patternVars :: DCEPat -> [(Var.Canonical, ExprFacts)]
patternVars (A.A facts pat) =
  case pat of
    (Pat.Data _ args) ->
      concatMap patternVars args

    (Pat.Record vars) ->
      map (\x -> (Var.Canonical Var.Local x, facts)) vars

    (Pat.Alias v subPat) ->
      [(Var.Canonical Var.Local v, facts)] ++ patternVars subPat

    (Pat.Var v) ->
      [(Var.Canonical Var.Local v, facts)] 

    Pat.Anything -> []

    (Pat.Literal _) -> []


-- Insert an edge from the first to the second given nodes
-- into the given graph
insertEdge :: RefNode -> RefNode -> RefGraph -> RefGraph
insertEdge ident newEdge dict =
  case Map.lookup ident dict of
    Nothing ->
      Map.insert ident (Set.singleton newEdge) dict 

    Just oldEdges ->
      Map.insert ident (Set.insert newEdge oldEdges) dict


-- Insert multiple edges into a graph
insertEdges :: RefNode -> (Set.Set RefNode) -> RefGraph -> RefGraph
insertEdges ident newEdges dict =
  case Map.lookup ident dict of
    Nothing ->
      Map.insert ident newEdges dict 

    Just oldEdges ->
      Map.insert ident (Set.union newEdges oldEdges) dict


-- Given two graphs, create a new graph with their nodes
-- and edges combined
graphUnion :: RefGraph -> RefGraph -> RefGraph
graphUnion g h =
  foldr
  (\(node,edgeSet) currentGraph -> insertEdges node edgeSet currentGraph )
  g $ Map.toList h


-- Apply a graph generating function to a list of inputs
-- then union the resulting graphs
unionMap :: (a -> RefGraph) -> [a] -> RefGraph
unionMap f inList =
  foldr graphUnion Map.empty $ map f inList


-- Create a graph containing one node and no edges
insertNode :: RefNode -> RefGraph
insertNode n =
  Map.fromList [(n, Set.empty)]


{- Constructing the refernce graph:
An expression refers to any variables it contains,
and a variable refers to the expression which was used to define it,
i.e. as the RHS of a let-clause,

Some special cases: function parameters are always sinks in the graph,
since they are defined entirely by the parameters during function application.

-}
makeRefGraph :: Module.Name -> RefEnv -> Int -> DCEExpr  -> RefGraph
makeRefGraph thisModule env currentDef (A.A ann expr) =
  let
    self =
      makeRefGraph thisModule env currentDef
  in
    case expr of
      Crash _ ->
        Map.empty
    
      Literal _ ->
        Map.empty
    
      Var v ->
        case Var.home v of
          Var.Local ->
            case Map.lookup v env of
              Just info ->
                Map.fromList
                  [(ExprNode currentDef,
                    Set.singleton $ InternalVar v info)]

          Var.Module someMod | someMod == thisModule ->
            case Map.lookup v env of
              Just info ->
                Map.fromList [(ExprNode currentDef, Set.singleton $ InternalVar v info)]
          _ -> 
            Map.fromList [( ExprNode currentDef, Set.singleton $ ExternalVar v)]
          
      Range sub1 sub2 ->
        self sub1 `graphUnion` self sub2

      ExplicitList subs ->
        unionMap self subs

      Binop op arg1 arg2 ->
        self arg1
        `graphUnion`
        self arg2
        `graphUnion`
        (self $ A.A ann $ Var op)

      Lambda pat arg -> 
        let
          newEnv =
            foldr
            (\(v, varAnn) currentEnv ->
              Map.insert v (exprIdent varAnn, exprRegion varAnn) currentEnv )
            env $ patternVars pat

          patPairs =
            [(v, (exprIdent patAnn, exprRegion patAnn)) | (v,patAnn) <- patternVars pat ]

          insertVarsGraph =
            unionMap (\(v, pr ) -> insertNode $ InternalVar v pr) $ patPairs
        in
          (makeRefGraph thisModule newEnv currentDef arg)
          `graphUnion` insertVarsGraph

      App sub1 sub2 ->
        self sub1 `graphUnion` self sub2

      MultiIf branches finalBranch ->
        unionMap (\ (c, e) -> self c `graphUnion` self e ) branches
        `graphUnion` self finalBranch

      Let defs body -> 
        let
          defPairs  =
            [(v, (exprIdent patAnn, exprRegion patAnn)) |
               (Def pat _ _ ) <- defs
               , (v, patAnn) <- patternVars pat]

          defExpr (Def _ rhs _ ) = rhs

          newEnv = 
            foldr (\(v, ident) currentEnv ->
                    Map.insert v ident currentEnv) env
                  defPairs

          defEdges =
            unionMap (\rhs@(A.A subAnn _ ) ->
                       makeRefGraph thisModule newEnv (exprIdent subAnn) rhs )
                     (map defExpr defs)

          patToRHSEdges =
            unionMap (\(v, (rhsId, reg )) ->
                       Map.fromList [(InternalVar v (rhsId, reg ), Set.singleton $ DefNode rhsId)] )
            defPairs

          insertRHSGraph =
            insertNode $ DefNode $ exprIdent ann

          insertVarsGraph =
            unionMap (\(v, pr ) -> insertNode $ InternalVar v pr) defPairs

          bodyEdges =
            makeRefGraph thisModule newEnv currentDef body
        in
          defEdges
          `graphUnion` bodyEdges
          `graphUnion` patToRHSEdges
          `graphUnion` insertRHSGraph
          `graphUnion` insertVarsGraph
        

      Case cexp branches ->
        let
          cexpEdges =
            self cexp

          patPairs =
            concatMap (\(pat, _) ->
              [(v, (exprIdent patAnn, exprRegion patAnn)) | (v,patAnn) <- patternVars pat ] )
              branches

          subEnv pat =
            (foldr (\ (x, patAnn) currentEnv ->
                     Map.insert x (exprIdent ann, exprRegion patAnn) currentEnv ) env $
                      patternVars pat )

          branchEdges (pat, bexpr) =
            makeRefGraph thisModule (subEnv pat) currentDef bexpr

          insertVarsGraph =
            unionMap (\(v, pr ) -> insertNode $ InternalVar v pr) $ patPairs
          
        in
         cexpEdges
         `graphUnion` (unionMap branchEdges branches)
         `graphUnion` insertVarsGraph

      Data _ctor args ->
        unionMap self args

      Access recExp _ ->
        self recExp

      Remove recExp _ ->
        self recExp

      Insert recExp _ newExp ->
        self recExp `graphUnion` self newExp

      Modify recExp subs ->
        self recExp `graphUnion` unionMap self (map snd subs)

      Record fieldPairs ->
        unionMap self $ map snd fieldPairs

      Port (In _ _) ->
        Map.empty

      Port (Out _ subEx _) ->
        self subEx

      Port (Task _ subEx _) ->
        self subEx

      GLShader _ _ _ ->
        Map.empty


-- Create a list, pairing variables, the unique identifier of the expression on their RHS
-- and the region of code they lie in
makeDefPairs :: Def -> [(Var.Canonical, (Int, R.Region))]
makeDefPairs (Def pat (A.A subAnn _) _ ) =
  [(v, (exprIdent subAnn, exprRegion pFacts)) | (v, pFacts) <- patternVars pat]


-- Given a the name and AST for a module, generate nodes
-- for all of the top-level names defined in that module
topLevelNames :: Module.Name -> DCEExpr -> [RefNode]
topLevelNames thisModule (A.A _ e) =
  case e of
    Var (Var.Canonical Var.BuiltIn nm) | nm == saveEnvName ->
      []

    Let defs expr ->
      let
        defPairs =
          concatMap makeDefPairs defs

        subNames =
          topLevelNames thisModule expr

        defNodes = map (\(v, pr) -> InternalVar v pr) defPairs
      in
        subNames ++ defNodes

    _ ->
      error "Shouldn't have non Let in top-level structure"  


-- Given the AST for a module, traverse the top-level Let structure
-- to generate the full reference graph for the module
traverseTopLevels
 :: Module.Name
 -> RefEnv
 -> DCEExpr
 -> RefGraph
traverseTopLevels thisModule env (A.A _ e) =
  case e of
    Var (Var.Canonical Var.BuiltIn nm) | nm == saveEnvName ->
      Map.empty

    Let defs body ->
      let
        defPairs =
          concatMap makeDefPairs defs

        defBodies =
          map (\(Def _ rhs@(A.A subAnn _) _ )
               -> (rhs, exprIdent subAnn) ) defs 

        initialEnv =
          List.foldr (\(v, pr ) currentEnv
                      -> Map.insert v pr currentEnv  ) env defPairs

        bodyGraph =
          traverseTopLevels thisModule initialEnv body

        defGraphs =
          unionMap (\(bdy,ident)
                    -> makeRefGraph thisModule initialEnv ident bdy) defBodies
        --Edges from our initial node to exported names

        initialEdges =
          unionMap
            (\(v, pr) ->
              Map.insert InitialNode (Set.singleton $ InternalVar v pr) Map.empty )
            defPairs
        --Edges from each defined name to the expression defining its value

        nameEdges =
          unionMap
            (\(v, (ident, reg)) ->
              Map.insert (InternalVar v (ident, reg) ) (Set.singleton $ ExprNode ident) Map.empty )
            defPairs
      in 
        defGraphs
        `graphUnion`
        bodyGraph
        `graphUnion`
        initialEdges
        `graphUnion`
        insertNode InitialNode
        `graphUnion`
        nameEdges
    _ ->
      error "Shouldn't have non Let in top-level structure"  


-- Generate the Data.Graph structure for a module's reference graph
moduleRefGraph
  :: Module.Name
  -> DCEExpr
  -> (G.Graph, G.Vertex -> (RefNode, [RefNode]), RefNode -> Maybe G.Vertex)
moduleRefGraph thisModule e =
  let
    rawEdgeGraph =
      traverseTopLevels thisModule Map.empty e

    --We need to add nodes that only occur in edges
    allNodes =
      (Set.fromList $ Map.keys rawEdgeGraph)
        `Set.union`
        (Set.unions $ Map.elems rawEdgeGraph)

    edgeGraph =
      Set.foldr (\n mapSoFar ->
                  if (Map.member n mapSoFar )
                  then mapSoFar
                  else Map.insert n Set.empty mapSoFar ) rawEdgeGraph allNodes

    (nodes, sets) =
      unzip $ Map.toList edgeGraph

    edgeLists =
      map Set.toList sets

    graphAsList =
      zip3 nodes nodes edgeLists

    (ggraph, getNode , getInt) =
      G.graphFromEdges graphAsList
  in
    (ggraph, (\(x,_,z) -> (x,z) ) . getNode, getInt)


-- The exposed interface for DCE
-- Given a module, generate the reference-graph for top-level defs,
-- monadically warning about any unused variables
analyzeModule
  :: Module.CanonicalModule
  -> Result.Result Warning.Warning Error.Error
      [(([String], String),
        [([String], String)])]
analyzeModule modul = 
  let
    
    identExpr =
      State.evalState (addExprIdent $ Module.program $ Module.body modul) 1

    names =
      (Module.names modul)
    
    (ggraph, getNode, getInt) = 
      moduleRefGraph names $ identExpr

    allNodes = 
      List.map (fst . getNode) $ G.vertices ggraph

    exportedNodes =
      topLevelNames names identExpr
    
    initialInts = 
      Maybe.catMaybes $ List.map getInt $ [InitialNode]

    reachableNodes = 
      Set.fromList $ concatMap Tree.flatten $ G.dfs ggraph initialInts

    varIsUnused vnode =
      case (vnode, getInt vnode) of
        (InternalVar _ _, Just v) -> 
          not $ Set.member v reachableNodes
        _ -> False

    makeWarning (InternalVar v (_, reg)) =  
      (reg, Warning.UnusedName v)

    unusedWarnings =  
      map makeWarning $ List.filter varIsUnused allNodes

    isTopLevelInt vi =
      case (fst $ getNode vi) of
        ExternalVar _ ->
          True

        x ->
          x `elem` exportedNodes

    getNodeVar nd =
      case nd of
        ExternalVar v -> v

        InternalVar v _ -> v

    importRefsForNode vnode = 
      case (getInt vnode) of
        Just vi ->
          List.map (getNodeVar . fst . getNode) $
            List.filter isTopLevelInt $ G.reachable ggraph vi

    varToPair thisModule v = case v of
      Var.Canonical Var.Local nm ->
        (thisModule, nm)

      Var.Canonical (Var.Module nms) nm ->
        (nms, nm)

    importExportRefs =
       List.map
         (\vnode@(InternalVar v _ ) ->
           (varToPair names v,
            map (varToPair names) $ importRefsForNode vnode) ) exportedNodes

  in  
    do  forM_ unusedWarnings (uncurry Result.warn)
        return importExportRefs


-- Given a list of reference graphs for different modules,
-- and a list of "target" values to be compiled,
-- return the list of internal and imported values that
-- the targets depend on
reachableImports
  :: [([String], [(([String], String), [([String], String)])])]
  -> [([String], String)]
  -> [([String], String)]
reachableImports inputGraphs exposedNames =
  let
    initialNode =
      ([], "--InitialNode")

    initialEdges =
      [(initialNode, initialNode, exposedNames )]

    graphEdges =
      do  (_, edgeList) <- inputGraphs
          (inNode, outNodes) <- edgeList
          return $ (inNode, inNode, outNodes)

    (moduleGraph, vertFn, keyFn ) =
         G.graphFromEdges $ initialEdges ++ graphEdges

    reachableNodes =
      G.reachable moduleGraph (Maybe.fromJust $ keyFn initialNode)
      
  in
    List.filter (/= initialNode) $ map ((\(x,_,_) -> x ) . vertFn) reachableNodes
