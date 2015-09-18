{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Sort (definitions) where

import Control.Monad.State as State
import Control.Applicative ((<$>),(<*>))
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.List as List

import AST.Expression.General (Expr'(..), PortImpl(..))
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Pattern as P
import qualified AST.Variable as V
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R

import qualified Canonicalize.Result as Result


data MaybeLambda = LambdaRef | Direct
  

--State during our traversal
--Store free variables we find, along with whether
--  we find them in a Lambda or not.
--Also store any circular reference errors we see

data GraphState =
  GraphState
  { lambdaMap :: Map.Map V.Canonical MaybeLambda
  , errors :: [[(String, R.Region)]]
  }


startState = GraphState Map.empty []

stateUnion :: GraphState -> GraphState -> GraphState
stateUnion s1 s2 =
  GraphState
    { lambdaMap = Map.union (lambdaMap s1) (lambdaMap s2)
    , errors = errors s1 ++ errors s2
    }

--Helpful functions for manipulating our state

addFreeVar :: V.Canonical -> MaybeLambda -> GraphState -> GraphState
addFreeVar v ml gs =
  let
    directIfEither l1 l2 =
      case (l1, l2) of
        (LambdaRef, LambdaRef) ->
          LambdaRef

        _ ->
          Direct
  in
   case Map.lookup v (lambdaMap gs) of
        Nothing ->
          gs {lambdaMap = Map.insert v ml $ lambdaMap gs}
        Just ml' ->
          gs {lambdaMap = Map.insert v (directIfEither ml ml') $ lambdaMap gs}

addError :: [(String, R.Region)] -> State GraphState ()
addError err =
  modify (\gs -> gs {errors = err : (errors gs)} )


-- STARTING POINT

definitions :: Canonical.Expr -> Result.ResultErr Canonical.Expr
definitions expression =
  let
    (result, _) = runState (reorder Set.empty expression) startState
  in Result.ok result


free :: V.Canonical -> MaybeLambda -> State GraphState ()
free v ml =
  modify $ addFreeVar v ml

{-
freeIfLocal :: V.Canonical -> State GraphState ()
freeIfLocal (V.Canonical home name) =
  case home of
    V.Local -> free name
    V.TopLevel _ -> free name
    V.BuiltIn -> return ()
    V.Module _ -> return ()
-}

-- REORDER EXPRESSIONS

reorder :: Set.Set V.Canonical -> Canonical.Expr -> State GraphState Canonical.Expr
reorder context (A.A ann expression) =
    A.A ann <$>
    case expression of
      -- Be careful adding and restricting freeVars
      Var var ->
          do  free var $ if Set.member var context then Direct else LambdaRef
              return expression

      Lambda pattern body ->
          uncurry Lambda <$> bindingReorder context (pattern,body)

      Binop op leftExpr rightExpr ->
          do  free op $ if Set.member op context then Direct else LambdaRef
              Binop op <$> reorder context leftExpr <*> reorder context rightExpr

      Case expr cases ->
          Case <$> reorder context expr <*> mapM (bindingReorder context) cases

      Data name exprs ->
          do  free (V.local name) LambdaRef --Data is never recursive
              Data name <$> mapM (reorder context) exprs

      -- Just pipe the reorder though
      Literal _ ->
          return expression

      Range lowExpr highExpr ->
          Range <$> reorder context lowExpr <*> reorder context highExpr

      ExplicitList es ->
          ExplicitList <$> mapM (reorder context) es

      App func arg ->
          App <$> reorder context func <*> reorder context arg

      If branches finally ->
          If
            <$> mapM (\(cond,branch) -> (,) <$> reorder context cond <*> reorder context branch) branches
            <*> reorder context finally

      Access record field ->
          Access
            <$> reorder context record
            <*> return field

      Update record fields ->
          Update
            <$> reorder context record
            <*> mapM (\(field,expr) -> (,) field <$> reorder context expr) fields

      Record fields ->
          Record
            <$> mapM (\(field,expr) -> (,) field <$> reorder context expr) fields

      GLShader _ _ _ ->
          return expression

      Port impl ->
          Port <$>
            case impl of
              In _ _ ->
                  return impl

              Out name expr tipe ->
                  (\e -> Out name e tipe) <$> reorder context expr

              Task name expr tipe ->
                  (\e -> Task name e tipe) <$> reorder context expr

      Crash _ ->
          return expression

      -- Actually do some reordering
      Let defs body ->
          do  body' <- reorder context body

              -- Sort defs into strongly connected components.This
              -- allows the programmer to write definitions in whatever
              -- order they please, we can still define things in order
              -- and generalize polymorphic functions when appropriate.
              sccs <- Graph.stronglyConnComp <$> buildDefGraph context defs
              let defss = map Graph.flattenSCC sccs

              -- remove let-bound variables from the context
              forM_ defs $ \(Canonical.Definition _ pattern _ _) -> do
                  bound pattern
                  mapM (\v -> free v Direct) (List.map V.local $ ctors pattern)

              let (A.A _ let') =
                    foldr (\ds bod -> A.A ann (Let ds bod)) body' defss

              return let'


ctors :: P.CanonicalPattern -> [String]
ctors (A.A _ pattern) =
    case pattern of
      P.Var _ ->
          []

      P.Alias _ p ->
          ctors p

      P.Record _ ->
          []

      P.Anything ->
          []

      P.Literal _ ->
          []

      P.Data (V.Canonical home name) ps ->
          case home of
            V.Local -> name : rest
            V.TopLevel _ -> name : rest
            V.BuiltIn -> rest
            V.Module _ -> rest
          where
            rest = concatMap ctors ps


bound :: P.CanonicalPattern -> State GraphState ()
bound pattern =
  let
    boundVars = Set.map (V.Canonical V.Local ) $ P.boundVarSet pattern
    stateDiff gs =
      gs {lambdaMap = Map.filterWithKey (\v _ -> not $ Set.member v boundVars ) $ lambdaMap gs}
  in
      modify stateDiff


bindingReorder
    :: Set.Set V.Canonical
    -> (P.CanonicalPattern, Canonical.Expr)
    -> State GraphState (P.CanonicalPattern, Canonical.Expr)
bindingReorder context (pattern, expr) =
  do  expr' <- reorder context expr
      bound pattern
      mapM (\v -> free v Direct) (List.map V.local $ ctors pattern)
      return (pattern, expr')


-- BUILD DEPENDENCY GRAPH BETWEEN DEFINITIONS

-- This also reorders the all of the sub-expressions in the Def list.
buildDefGraph
    :: Set.Set V.Canonical
    -> [Canonical.Def]
    -> State GraphState [(Canonical.Def, Int, [Int])]
buildDefGraph context defs =
  do  pdefsDeps <- mapM (reorderAndGetDependencies context) defs
      return $ realDeps (addKey pdefsDeps)
  where
    addKey :: [(Canonical.Def, [V.Canonical])] -> [(Canonical.Def, Int, [V.Canonical])]
    addKey =
        zipWith (\n (pdef,deps) -> (pdef,n,deps)) [0..]

    variableToKey :: (Canonical.Def, Int, [V.Canonical]) -> [(String, Int)]
    variableToKey (Canonical.Definition _ pattern _ _, key, _) =
        [ (var, key) | var <- P.boundVarList pattern ]

    variableToKeyMap :: [(Canonical.Def, Int, [V.Canonical])] -> Map.Map String Int
    variableToKeyMap pdefsDeps =
        Map.fromList (concatMap variableToKey pdefsDeps)

    

    realDeps :: [(Canonical.Def, Int, [V.Canonical])] -> [(Canonical.Def, Int, [Int])]
    realDeps pdefsDeps =
        map convert stringDeps
      where
        --TODO is this right?
        stringDeps = map (\(a,b,vars) -> (a,b, map V.name vars) )
                     pdefsDeps
        varDict = variableToKeyMap pdefsDeps
        convert (pdef, key, deps) =
            (pdef, key, Maybe.mapMaybe (flip Map.lookup varDict) deps)


reorderAndGetDependencies
    :: Set.Set V.Canonical
    -> Canonical.Def
    -> State GraphState (Canonical.Def, [V.Canonical])
reorderAndGetDependencies context (Canonical.Definition facts pattern expr mType) =
  do  globalFrees <- get
      -- work in a fresh environment
      put startState --TODO this or empty?
      expr' <- reorder context expr
      localFrees <- (Set.fromList . Map.keys . lambdaMap) <$> get
      -- merge with global frees
      modify (stateUnion globalFrees)
      return (Canonical.Definition facts pattern expr' mType, Set.toList localFrees)
