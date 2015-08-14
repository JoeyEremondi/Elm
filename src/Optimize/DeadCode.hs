
module Optimize.DeadCode where

import AST.Expression.General
import qualified AST.Variable as Var
import qualified AST.Pattern as Pat
import qualified AST.Module as Module
--import qualified AST.Traversals as ASTT
--import qualified AST.Expression.Canonical as Canon
import qualified AST.Expression.Optimized as Opt
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

import Control.Monad (forM, mapM, forM_)
import Control.Applicative ((<$>), (<*>) )

import Debug.Trace (trace)

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


data ExprFacts =
  ExprFacts
  { exprRegion :: R.Region
  , exprIdent :: Int
  } deriving (Eq, Ord, Show)
  

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


insertEdge :: RefNode -> RefNode -> RefGraph -> RefGraph
insertEdge ident newEdge dict =
  case Map.lookup ident dict of
    Nothing ->
      Map.insert ident (Set.singleton newEdge) dict 

    Just oldEdges ->
      Map.insert ident (Set.insert newEdge oldEdges) dict


insertEdges :: RefNode -> (Set.Set RefNode) -> RefGraph -> RefGraph
insertEdges ident newEdges dict =
  case Map.lookup ident dict of
    Nothing -> Map.insert ident newEdges dict 
    Just oldEdges -> Map.insert ident (Set.union newEdges oldEdges) dict


graphUnion :: RefGraph -> RefGraph -> RefGraph
graphUnion g h =
  foldr (\(node,edgeSet) currentGraph -> insertEdges node edgeSet currentGraph ) g $ Map.toList h


unionMap :: (a -> RefGraph) -> [a] -> RefGraph
unionMap f inList =
  foldr graphUnion Map.empty $ map f inList



insertNode :: RefNode -> RefGraph
insertNode n = Map.fromList [(n, Set.empty)]


type DCEExpr = Gen.Expr ExprFacts Def Var.Canonical Type.Canonical

type DCEExpr' = Gen.Expr' ExprFacts Def Var.Canonical Type.Canonical


type DCEPat = Pat.Pattern ExprFacts Var.Canonical


data Def
    = Def Opt.Facts DCEPat DCEExpr
    deriving (Show)

{- Constructing the refernce graph:
An expression refers to any variables it contains,
and a variable refers to the expression which was used to define it,
i.e. as the RHS of a let-clause,


Some special cases: function parameters are always sinks in the graph,
since they are defined entirely by the parameters during function application.

For case statements, we treat the variables defined by matching as dead-ends,
and assume that every variable in the expression being matched
is referenced by the body of each branch of the case statement.
-}
makeRefGraph :: Module.Name -> RefEnv -> Int -> DCEExpr  -> RefGraph
makeRefGraph thisModule env currentDef (A.A ann expr) = 
  case (trace ("makeGraph top, env\n" ++ show env) $ expr) of
    Crash _ ->
      Map.empty
    
    (Literal _) ->
      Map.empty
    
    (Var v) ->
      case Var.home v of
        Var.Local ->
          case Map.lookup v env of
            Just info ->
              Map.fromList
                [(ExprNode currentDef,
                  Set.singleton $ InternalVar v info)]
            _ ->
              error $
                "In module " ++ (show thisModule )
                ++ "\nVar " ++ (show v)
                ++ " not in env " ++ show env
                ++ "\nCurrent def: " ++ show currentDef
        Var.Module someMod | someMod == thisModule ->
          case Map.lookup v env of
            Just info ->
              Map.fromList [(ExprNode currentDef, Set.singleton $ InternalVar v info)]
        _ ->
          Map.fromList [( ExprNode currentDef, Set.singleton $ ExternalVar v)]
          
    (Range sub1 sub2) ->
      self sub1 `graphUnion` self sub2

    (ExplicitList subs) ->
      unionMap self subs

    (Binop _op arg1 arg2) ->
      self arg1 `graphUnion` self arg2

    (Lambda pat arg) -> trace "Lambda case" $ 
      let
        newEnv =
          foldr (\(v, varAnn) currentEnv -> Map.insert v (exprIdent varAnn, exprRegion varAnn) currentEnv ) env $ patternVars pat
        patPairs = trace ("Lambda pattern vars " ++ show (patternVars pat )) $
          [(v, (exprIdent patAnn, exprRegion patAnn)) | (v,patAnn) <- patternVars pat ]
        insertVarsGraph =
          unionMap (\(v, pr ) -> insertNode $ InternalVar v pr) $ patPairs
      in trace ("Lambda pattern vars " ++ show (patternVars pat )) $
        (makeRefGraph thisModule newEnv currentDef arg)
        `graphUnion` insertVarsGraph

    (App sub1 sub2) ->
      self sub1 `graphUnion` self sub2

    (MultiIf branches finalBranch) ->
      unionMap (\ (c, e) -> self c `graphUnion` self e ) branches 

    (Let defs body) -> trace "Let case" $ 
      let
        defPairs (Def facts pat _ ) =
          [(v, (exprIdent patAnn, exprRegion patAnn)) | (v, patAnn) <- patternVars pat]
        defExpr (Def _ _ rhs ) = rhs
        newEnv =
          foldr (\(v, ident) currentEnv ->
                  Map.insert v ident currentEnv) env
                (concatMap defPairs defs)
        defEdges =
          unionMap (\rhs@(A.A subAnn _ ) ->
                     makeRefGraph thisModule newEnv (exprIdent subAnn) rhs )
                   (map defExpr defs)
        patToRHSEdges =
          unionMap (\(v, (rhsId, reg )) ->
                     Map.fromList [(InternalVar v (rhsId, reg ), Set.singleton $ DefNode rhsId)] )
          (concatMap defPairs defs)
        insertRHSGraph =
          insertNode $ DefNode $ exprIdent ann
        insertVarsGraph =
          unionMap (\(v, pr ) -> insertNode $ InternalVar v pr) $ concatMap defPairs defs
        bodyEdges = self body
      in
        defEdges
        `graphUnion` bodyEdges
        `graphUnion` patToRHSEdges
        `graphUnion` insertRHSGraph
        `graphUnion` insertVarsGraph
        

    (Case cexp branches) ->
      let
        cexpEdges = self cexp
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

    (Data _ctor args) ->
      unionMap self args

    (Access recExp _) ->
      self recExp

    (Remove recExp _) ->
      self recExp

    (Insert recExp _ newExp) ->
      self recExp `graphUnion` self newExp

    (Modify recExp subs) ->
      self recExp `graphUnion` unionMap self (map snd subs)

    (Record fieldPairs) ->
      unionMap self $ map snd fieldPairs

    (Port (In _ _)) ->
      Map.empty

    (Port (Out _ subEx _)) ->
      self subEx

    (Port (Task _ subEx _)) ->
      self subEx

    (GLShader _ _ _ ) ->
      Map.empty
      
  where self = makeRefGraph thisModule env currentDef


dropPatAnnotation :: DCEPat -> Pat.Optimized
dropPatAnnotation (A.A facts pat) =
  A.A (exprRegion facts ) $
    case pat of
      Pat.Data p1 p2 ->
        Pat.Data p1 (map dropPatAnnotation p2)

      Pat.Record p ->
        Pat.Record p

      Pat.Alias p1 p2 ->
        Pat.Alias p1 $ dropPatAnnotation p2

      Pat.Var p ->
        Pat.Var p

      Pat.Anything ->
        Pat.Anything

      Pat.Literal p ->
        Pat.Literal p


dropDefAnnotation :: Def -> Opt.Def
dropDefAnnotation (Def facts pat expr ) =
  Opt.Definition facts (dropPatAnnotation pat) (dropAnnotation expr)

dropAnnotation ::
  DCEExpr -> Opt.Expr
dropAnnotation (A.A facts expr ) =
  A.A (exprRegion facts ) $
    case expr of
      Literal e ->
        Literal e

      Var e ->
        Var e

      Range e1 e2 ->
        Range (dropAnnotation e1) (dropAnnotation e2)

      ExplicitList e ->
        ExplicitList $ map dropAnnotation e

      Binop e1 e2 e3 ->
        Binop e1 (dropAnnotation e2) (dropAnnotation e3)

      Lambda e1 e2 ->
        Lambda (dropPatAnnotation e1) (dropAnnotation e2)

      App e1 e2 ->
        App (dropAnnotation e1) (dropAnnotation e2)

      MultiIf e1 e2 ->
        MultiIf (map (\(x,y) -> (dropAnnotation x, dropAnnotation y)) e1) (dropAnnotation e2)

      Let defs e2 ->
        Let (map dropDefAnnotation defs ) (dropAnnotation e2 )

      Case e1 e2 ->
        Case (dropAnnotation e1) $ map (\(x,y) -> (dropPatAnnotation x, dropAnnotation y) ) e2

      Data e1 e2 ->
        Data e1 $ map dropAnnotation e2

      Access e1 e2 ->
        Access (dropAnnotation e1) e2

      Remove e1 e2 ->
        Remove (dropAnnotation e1) e2

      Insert e1 e2 e3 ->
        Insert (dropAnnotation e1) e2 (dropAnnotation e3)

      Modify e1 e2 ->
        Modify (dropAnnotation e1) (map (\(x,y) -> (x, dropAnnotation y) ) e2)

      Record e ->
        Record (map (\(x,y) -> (x, dropAnnotation y) ) e)

      Port (In p1 p2) ->
        Port $ In p1 p2
      
      Port (Out p1 p2 p3) ->
        Port $ Out p1 (dropAnnotation p2) p3

      Port (Task p1 p2 p3) ->
        Port $ Task p1 (dropAnnotation p2) p3

      GLShader e1 e2 e3 ->
        GLShader e1 e2 e3

      Crash e ->
        Crash e

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
        makeDefPairs (Def facts pat (A.A subAnn _) ) =
          [(v, (exprIdent subAnn, exprRegion pFacts)) | (v, pFacts) <- patternVars pat]
        defPairs =
          concatMap makeDefPairs defs
        defBodies =
          map (\(Def _ _ rhs@(A.A subAnn _) ) -> (rhs, exprIdent subAnn) ) defs 
        initialEnv =
          List.foldr (\(v, pr ) currentEnv -> Map.insert v pr currentEnv  ) env defPairs
        bodyGraph =
          traverseTopLevels thisModule initialEnv body
        defGraphs =
          unionMap (\(bdy,ident) -> makeRefGraph thisModule initialEnv ident bdy) defBodies
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
      in trace ("TL Let defs: " ++ show (map show defs) ) $
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

--TODO: is ExportedIdent wrong?               
moduleRefGraph
  :: Module.Name
  -> DCEExpr
  -> (G.Graph, G.Vertex -> (RefNode, [RefNode]), RefNode -> Maybe G.Vertex)
moduleRefGraph thisModule e =
  let
    edgeGraph =
      traverseTopLevels thisModule Map.empty e
    (nodes, sets) =
      unzip $ Map.toList edgeGraph
    edgeLists =
      map Set.toList sets
    graphAsList =
      zip3 nodes nodes edgeLists
    (ggraph, getNode , getInt) =
      G.graphFromEdges graphAsList
  in (ggraph, (\(x,_,z) -> (x,z) ) . getNode, getInt)


addDefIdent :: Opt.Def -> State.State Int Def
addDefIdent (Opt.Definition facts pat expr) =
  do  nextInt <- State.get
      State.put (nextInt + 1)
      let newFacts = facts {Opt.defIdent = nextInt}
      Def newFacts <$> addPatIdent pat <*> addUniqueIdent expr


addPortIdent :: PortImpl Opt.Expr t -> State.State Int (PortImpl DCEExpr t)
addPortIdent portImpl =
    case portImpl of 
      In p1 p2 ->
        return $ In p1 p2
      
      Out p1 p2 p3 -> do
        newExpr <- addUniqueIdent p2
        return $ Out p1 newExpr p3
      
      Task p1 p2 p3 -> do
        newExpr <- addUniqueIdent p2
        return $ Task p1 newExpr p3


addPatIdent :: Pat.Optimized -> State.State Int DCEPat
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

addUniqueIdent :: Opt.Expr -> State.State Int DCEExpr
addUniqueIdent (A.A reg e) =
  do let
       self = addUniqueIdent

       newExprM :: State.State Int DCEExpr'
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
         
           Lambda pat body -> trace ("LAMBDA: Adding unique ident to " ++ show e) $
             do  newPat <- addPatIdent pat
                 error $ "Lambda access " ++ show e --Lambda newPat <$> self body
         
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

showTop (A.A _ e) = case e of
  Let defs body ->
    show defs ++ " " ++ showTop body
  _ -> ""


analyzeModule
  :: Module.Optimized
  -> Result.Result Warning.Warning Error.Error
      ( Module.Optimized, [(Var.Canonical, [Var.Canonical])])
analyzeModule modul = 
  let
    
    identExpr =
      State.evalState (addUniqueIdent $ Module.program $ Module.body modul) 1
    
    (ggraph, getNode, getInt ) = trace ("ORIGINAL program: " ++ (showTop $ Module.program $ Module.body modul )) $
      moduleRefGraph (Module.names modul) $ identExpr

    allNodes = 
      List.map (fst . getNode) $ G.vertices ggraph

    exportedNodes = [InitialNode]
    
    initialInts = 
      Maybe.catMaybes $ List.map getInt $ exportedNodes
    --TODO: fast way to do this with arrays?

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

    isExternalInt vi =
      case (fst $ getNode vi) of
        ExternalVar _ -> True

        _ -> False

    importRefsForNode vnode =
      case (getInt vnode) of
        Nothing -> [] --TODO this case should be impossible

        Just vi ->
          List.map ((\(ExternalVar v ) -> v ) . fst . getNode) $
            List.filter isExternalInt $ G.reachable ggraph vi

    importExportRefs =
      Map.fromList $ List.map (\vnode@(InternalVar v _ ) ->
                                (v, importRefsForNode vnode) ) exportedNodes

    defIsReachable vnode =
      case (vnode, getInt vnode) of
        (ExprNode ident, Just varInt) ->
          if Set.member varInt reachableNodes
          then Just ident
          else Nothing

        _ ->
          Nothing

    reachableDefs =
      Set.fromList $ Maybe.catMaybes $ map defIsReachable allNodes

    newProgram = --TODO clean up program?
      Module.program $ Module.body modul
    --newProgram =
    --  (error "TODO thread fn") (removeUnusedDefs reachableDefs) $ Module.program $ Module.body modul

  in
    do  forM_ unusedWarnings (uncurry Result.warn)
        return $
          ( modul {Module.body = (Module.body modul) {Module.program = newProgram } }
          , [] ) {-"TODO import export refs"-}



removeUnusedDefs
  :: Set.Set Int
  -> DCEExpr
  -> DCEExpr
removeUnusedDefs usedDefs e@(A.A ann expr) =
  case expr of
    Let defs body ->
      let
        isUsed (Def _ (A.A subAnn _) _) =
          Set.member (exprIdent subAnn ) usedDefs
      in
        A.A ann $ Let (filter isUsed defs) body
    _ -> e


reachableImports
  :: [([String], [(Var.Canonical, [Var.Canonical])])]
  -> [Var.Canonical]
  -> [Var.Canonical]
reachableImports inputGraphs exposedNames =
  let
    fixName modName var =
      case var of
        Var.Canonical Var.Local nm ->
          Var.Canonical (Var.Module modName ) nm

        _ ->
          var
    
    initialNode =
      Var.Canonical Var.BuiltIn "--InitialNode"

    initialEdges =
      [(initialNode, initialNode, exposedNames )]

    graphEdges =
      do  (modulNames, edgeList) <- inputGraphs
          (inNode, outNodes) <- edgeList
          let fixedInNode = fixName modulNames inNode
              fixedOutNodes = map (fixName modulNames) outNodes
          return $ (fixedInNode, fixedInNode, fixedOutNodes)

    (moduleGraph, vertFn, keyFn ) =
         G.graphFromEdges $ initialEdges ++ graphEdges
         

    initialNodes = map (Maybe.fromJust . keyFn) exposedNames

    reachableNodes =
      G.reachable moduleGraph (Maybe.fromJust $ keyFn initialNode)
      
  in
    List.filter (/= initialNode) $ map ((\(x,y,z) -> x ) . vertFn) reachableNodes
