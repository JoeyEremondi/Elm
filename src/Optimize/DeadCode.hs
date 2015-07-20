
module Optimize.DeadCode where

import AST.Expression.General
import qualified AST.Variable as Var
import qualified AST.Pattern as Pat
import qualified AST.Expression.Canonical as Canon
import qualified Reporting.Annotation as A

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Graph as G

data RefNode =
  ExternalVar Var.Canonical
  | InternalVar Var.Canonical Int
  | ExprNode Int
  deriving (Eq, Ord)


type RefEdge = (RefNode, RefNode)


type RefEnv = Map.Map Var.Canonical Int


type RefGraph = Map.Map RefNode (Set.Set RefNode)


patternVars :: Pat.CanonicalPattern -> [Var.Canonical]
patternVars (A.A _ pat) =
  case pat of
    (Pat.Data _ args) ->
      concatMap patternVars args

    (Pat.Record vars) ->
      map (Var.Canonical Var.Local) vars

    (Pat.Alias v subPat) ->
      [Var.Canonical Var.Local v] ++ patternVars subPat

    (Pat.Var v) ->
      [Var.Canonical Var.Local v] 

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
          Map.fromList [(ExprNode currentDef, Set.singleton $ InternalVar v $ env Map.! v)]
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
        newEnv = foldr (\v currentEnv -> Map.insert v (A.ident ann) currentEnv ) env $ patternVars pat
      in
        makeRefGraph thisModule newEnv currentDef arg

    (App sub1 sub2) ->
      self sub1 `graphUnion` self sub2

    (MultiIf branches) ->
      unionMap (\ (c, e) -> self c `graphUnion` self e ) branches 

    (Let defs body) ->
      let
        defPairs (Canon.Definition pat (A.A subAnn _) _ ) = [(v, A.ident subAnn) | v <- patternVars pat]
        defExpr (Canon.Definition _ rhs _ ) = rhs
        newEnv = foldr (\(v, ident) currentEnv -> Map.insert v ident currentEnv) env
                 $ concatMap defPairs defs
        defEdges = unionMap (\rhs@(A.A subAnn _ ) -> makeRefGraph thisModule newEnv (A.ident subAnn) rhs ) $ map defExpr defs
        bodyEdges = self body
      in
        defEdges `graphUnion` bodyEdges

    (Case cexp branches) ->
      let
        cexpEdges = self cexp
        subEnv pat = (foldr (\ x currentEnv -> Map.insert x (A.ident ann) currentEnv ) env $
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
               
