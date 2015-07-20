
module Optimize.DeadCode where

import AST.Expression.General
import qualified AST.Variable as Var
import qualified AST.Pattern as Pat
import qualified AST.Module as Module
import qualified AST.Expression.Canonical as Canon
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import qualified Reporting.Warning as Warning

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Graph as G
import qualified Data.Tree as Tree


data RefNode =
    ExternalVar Var.Canonical
  | InternalVar Var.Canonical (Int, R.Region)
  | ExprNode Int
  | DefNode Int
    deriving (Show, Eq, Ord)



type RefEdge = (RefNode, RefNode)


type RefEnv = Map.Map Var.Canonical (Int, R.Region)


type RefGraph = Map.Map RefNode (Set.Set RefNode)


patternVars :: Pat.CanonicalPattern -> [(Var.Canonical, R.Region)]
patternVars (A.A ann pat) =
  case pat of
    (Pat.Data _ args) ->
      concatMap patternVars args

    (Pat.Record vars) ->
      map (\x -> (Var.Canonical Var.Local x, A.region ann)) vars

    (Pat.Alias v subPat) ->
      [(Var.Canonical Var.Local v, A.region ann)] ++ patternVars subPat

    (Pat.Var v) ->
      [(Var.Canonical Var.Local v, A.region ann)] 

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


exportedIdent :: Int
exportedIdent = -99


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
makeRefGraph :: [String] -> RefEnv -> Int -> Canon.Expr -> RefGraph
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
        newEnv = foldr (\(v, reg) currentEnv -> Map.insert v (A.ident ann, reg) currentEnv ) env $ patternVars pat
      in
        makeRefGraph thisModule newEnv currentDef arg

    (App sub1 sub2) ->
      self sub1 `graphUnion` self sub2

    (MultiIf branches) ->
      unionMap (\ (c, e) -> self c `graphUnion` self e ) branches 

    (Let defs body) ->
      let
        defPairs (Canon.Definition pat (A.A subAnn _) _ ) =
          [(v, (A.ident subAnn, reg)) | (v, reg) <- patternVars pat]
        defExpr (Canon.Definition _ rhs _ ) = rhs
        newEnv =
          foldr (\(v, ident) currentEnv ->
                  Map.insert v ident currentEnv) env
                (concatMap defPairs defs)
        defEdges =
          unionMap (\rhs@(A.A subAnn _ ) ->
                     makeRefGraph thisModule newEnv (A.ident subAnn) rhs )
                   (map defExpr defs)
        patToRHSEdges =
          unionMap (\(v, (rhsId, reg )) ->
                     Map.fromList [(InternalVar v (rhsId, reg ), Set.singleton $ DefNode rhsId)] )
          (concatMap defPairs defs)
        bodyEdges = self body
      in
        defEdges `graphUnion` bodyEdges

    (Case cexp branches) ->
      let
        cexpEdges = self cexp
        subEnv pat =
          (foldr (\ (x, reg) currentEnv ->
                   Map.insert x (A.ident ann, reg) currentEnv ) env $
                    patternVars pat )
        branchEdges (pat, bexpr) =
          makeRefGraph thisModule (subEnv pat) currentDef bexpr
      in
       cexpEdges `graphUnion` (unionMap branchEdges branches)

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


--TODO: is ExportedIdent wrong?               
moduleRefGraph
  :: [String]
  -> Canon.Expr
  -> (G.Graph, G.Vertex -> (RefNode, [RefNode]), RefNode -> Maybe G.Vertex)
moduleRefGraph thisModule e@(A.A ann (Let defs body)) =
  let
    edgeGraph =
      makeRefGraph thisModule Map.empty exportedIdent e
    (nodes, sets) =
      unzip $ Map.toList edgeGraph
    edgeLists =
      map Set.toList sets
    graphAsList =
      zip3 nodes nodes edgeLists
    (ggraph, getNode , getInt) =
      G.graphFromEdges graphAsList
  in (ggraph, (\(x,_,z) -> (x,z) ) . getNode, getInt)


isExported :: [Var.Value] -> RefNode -> Bool
isExported exports node =
  case node of
    InternalVar (Var.Canonical Var.Local v ) (vid, _) | vid == exportedIdent ->
      Var.Value v `elem` exports
    _ -> False


analyzeModule
  :: [String]
  -> Module.CanonicalModule
  -> ( Module.CanonicalModule
     , [A.Located Warning.Warning]
     , Map.Map Var.Canonical [Var.Canonical])
analyzeModule modName modul =
  let
    (ggraph, getNode, getInt ) =
      moduleRefGraph modName $ Module.program $ Module.body modul

    allNodes =
      List.map (fst . getNode) $ G.vertices ggraph

    exportedNodes =
      List.filter (isExported (Module.exports modul)) allNodes

    initialInts =
      Maybe.catMaybes $ List.map getInt $ exportedNodes
    --TODO: fast way to do this with arrays?

    reachableNodes =
      Set.fromList $ concatMap Tree.flatten $ G.dfs ggraph initialInts

    varIsUnused vnode =
      case (vnode, getInt vnode) of
        (_, Nothing) -> False
        (InternalVar _ _, Just v) -> not $ Set.member v reachableNodes

    makeWarning (InternalVar v (_, reg)) = A.A reg $ Warning.UnusedName v

    unusedWarnings = map makeWarning $ List.filter varIsUnused allNodes

    isExternalInt vi =
      case (fst $ getNode vi) of
        ExternalVar v -> True
        _ -> False

    importRefsForNode vnode =
      case (getInt vnode) of
        Nothing -> [] --TODO this case should be impossible
        Just vi ->
          List.map ((\(ExternalVar v ) -> v ) . fst . getNode) $
            List.filter isExternalInt $ G.reachable ggraph vi

    importExportRefs =
      Map.fromList $ List.map (\vnode@(InternalVar v _ ) -> (v, importRefsForNode vnode) ) exportedNodes

  in
    (modul, unusedWarnings, importExportRefs)



