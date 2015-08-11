module Optimize.DeadCode where

import AST.Expression.General
import qualified AST.Variable as Var
import qualified AST.Pattern as Pat
import qualified AST.Module as Module
--import qualified AST.Traversals as ASTT
--import qualified AST.Expression.Canonical as Canon
import qualified AST.Expression.Optimized as Opt
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

import Control.Monad (forM, mapM, forM_)


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


patternVars :: Pat.Pattern Opt.ExprFacts Var.Canonical -> [(Var.Canonical, Opt.ExprFacts)]
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
makeRefGraph :: Module.Name -> RefEnv -> Int -> Opt.Expr -> RefGraph
makeRefGraph thisModule env currentDef (A.A ann expr) =
  case expr of
    (Literal _) ->
      Map.empty
    
    (Var v) ->
      case Var.home v of
        Var.Local ->
          Map.fromList
            [(ExprNode currentDef,
              Set.singleton $ InternalVar v $ env Map.! v)]
        Var.Module someMod | someMod == thisModule ->
          Map.fromList [(ExprNode currentDef, Set.singleton $ InternalVar v $ env Map.! v)]
        _ ->
          Map.fromList [( ExprNode currentDef, Set.singleton $ ExternalVar v)]
          
    (Range sub1 sub2) ->
      self sub1 `graphUnion` self sub2

    (ExplicitList subs) ->
      unionMap self subs

    (Binop _op arg1 arg2) ->
      self arg1 `graphUnion` self arg2

    (Lambda pat arg) ->
      let
        newEnv =
          foldr (\(v, varAnn) currentEnv -> Map.insert v (Opt.exprIdent varAnn, Opt.exprRegion varAnn) currentEnv ) env $ patternVars pat
        patPairs =
          [(v, (Opt.exprIdent patAnn, Opt.exprRegion patAnn)) | (v,patAnn) <- patternVars pat ]
        insertVarsGraph =
          unionMap (\(v, pr ) -> insertNode $ InternalVar v pr) $ patPairs
      in
        (makeRefGraph thisModule newEnv currentDef arg)
        `graphUnion` insertVarsGraph

    (App sub1 sub2) ->
      self sub1 `graphUnion` self sub2

    (MultiIf branches finalBranch) ->
      unionMap (\ (c, e) -> self c `graphUnion` self e ) branches 

    (Let defs body) ->
      let
        defPairs (Opt.Definition facts pat _ ) =
          [(v, (Opt.exprIdent patAnn, Opt.exprRegion patAnn)) | (v, patAnn) <- patternVars pat]
        defExpr (Opt.Definition _ _ rhs ) = rhs
        newEnv =
          foldr (\(v, ident) currentEnv ->
                  Map.insert v ident currentEnv) env
                (concatMap defPairs defs)
        defEdges =
          unionMap (\rhs@(A.A subAnn _ ) ->
                     makeRefGraph thisModule newEnv (Opt.exprIdent subAnn) rhs )
                   (map defExpr defs)
        patToRHSEdges =
          unionMap (\(v, (rhsId, reg )) ->
                     Map.fromList [(InternalVar v (rhsId, reg ), Set.singleton $ DefNode rhsId)] )
          (concatMap defPairs defs)
        insertRHSGraph =
          insertNode $ DefNode $ Opt.exprIdent ann
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
            [(v, (Opt.exprIdent patAnn, Opt.exprRegion patAnn)) | (v,patAnn) <- patternVars pat ] )
            branches
        subEnv pat =
          (foldr (\ (x, patAnn) currentEnv ->
                   Map.insert x (Opt.exprIdent ann, Opt.exprRegion patAnn) currentEnv ) env $
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


traverseTopLevels
 :: Module.Name
 -> RefEnv
 -> Opt.Expr
 -> RefGraph
traverseTopLevels thisModule env (A.A _ e) =
  case e of
    Var (Var.Canonical Var.BuiltIn nm) | nm == saveEnvName ->
      Map.empty
    Let defs body ->
      let
        makeDefPairs (Opt.Definition facts pat (A.A subAnn _) ) =
          [(v, (Opt.exprIdent subAnn, Opt.exprRegion pFacts)) | (v, pFacts) <- patternVars pat]
        defPairs =
          concatMap makeDefPairs defs
        defBodies =
          map (\(Opt.Definition _ _ rhs@(A.A subAnn _) ) -> (rhs, Opt.exprIdent subAnn) ) defs 
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

--TODO: is ExportedIdent wrong?               
moduleRefGraph
  :: Module.Name
  -> Opt.Expr
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



analyzeModule
  :: Module.Optimized
  -> Result.Result Warning.Warning Error.Error
      ( Module.CanonicalModule, Map.Map Var.Canonical [Var.Canonical])
analyzeModule modul = 
  let
    (ggraph, getNode, getInt ) =
      moduleRefGraph (Module.names modul) $ Module.program $ Module.body modul

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

    newProgram =
      (error "TODO thread fn") (removeUnusedDefs reachableDefs) $ Module.program $ Module.body modul

  in
    do  forM_ unusedWarnings (uncurry Result.warn)
        return $
          ( modul {Module.body = (Module.body modul) {Module.program = newProgram } }
          , importExportRefs)



removeUnusedDefs
  :: Set.Set Int
  -> Opt.Expr
  -> Opt.Expr
removeUnusedDefs usedDefs e@(A.A ann expr) =
  case expr of
    Let defs body ->
      let
        isUsed (Opt.Definition _ (A.A subAnn _) _) =
          Set.member (Opt.exprIdent subAnn ) usedDefs
      in
        A.A ann $ Let (filter isUsed defs) body
    _ -> e