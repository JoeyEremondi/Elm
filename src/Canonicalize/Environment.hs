{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Environment where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import AST.Expression.General (saveEnvName)
import qualified AST.Module as Module
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import Elm.Utils ((|>))


-- ENVIRONMENT

data Environment = Env
    { _home     :: Module.Name
    , _values   :: Dict Var.Canonical
    , _waiting  :: Dict Var.Canonical --Values that enter the env once we see a lambda
    , _adts     :: Dict Var.Canonical
    , _aliases  :: Dict (Var.Canonical, [String], Type.Canonical)
    , _patterns :: Dict (Var.Canonical, Int)
    }


type Dict a =
    Map.Map String (Set.Set a)


fromPatches :: Module.Name -> [Patch] -> Environment
fromPatches moduleName patches =
  addPatches
      patches
      (Env moduleName Map.empty Map.empty Map.empty Map.empty Map.empty)


addPattern :: P.Pattern ann var -> Environment -> Environment
addPattern pattern env =
  let patches =
        map (\x -> Value x (Var.local x)) (P.boundVarList pattern)
  in
      addPatches patches env


addPatternWaiting :: P.Pattern ann var -> Environment -> Environment
addPatternWaiting pattern env =
  let patches =
        map (\x -> Waiting x (Var.local x)) (P.boundVarList pattern)
  in
      addPatches patches env

clearWaiting :: Environment -> Environment
clearWaiting = addPatches [MoveFromWaiting]

-- PATCHES

data Patch
    = Value String Var.Canonical
    | Waiting String Var.Canonical
    | MoveFromWaiting
    | Union String Var.Canonical
    | Alias String (Var.Canonical, [String], Type.Canonical)
    | Pattern String (Var.Canonical, Int)


-- ADD PATCH TO ENVIRONMENT

addPatches :: [Patch] -> Environment -> Environment
addPatches patches env =
  List.foldl' (flip addPatch) env patches


addPatch :: Patch -> Environment -> Environment
addPatch patch env =
  case patch of
    Value name var ->
        env { _values = insert name var (_values env) }

    Waiting name var -> --Add a variable to waiting, shadow it in values
      env { _values = Map.delete name (_values env)
            , _waiting = insert name var (_waiting env) }

    MoveFromWaiting -> --Transfer all values from Waiting into Values
      env { _values = Map.union (_waiting env) (_values env)
            , _waiting = Map.empty }
      
    Union name var ->
        env { _adts = insert name var (_adts env) }

    Alias name var ->
        env { _aliases = insert name var (_aliases env) }

    Pattern name var ->
        env { _patterns = insert name var (_patterns env) }


insert :: (Ord a) => String -> a -> Dict a -> Dict a
insert key value =
  Map.insertWith Set.union key (Set.singleton value)


-- PATCH HELPERS

builtinPatches :: [Patch]
builtinPatches =
  concat
    [ map (patch Value) (tupleNames ++ [saveEnvName])
    , map (patch Union) (tupleNames ++ ["List","Int","Float","Char","Bool","String"])
    , map (patternPatch) (tuples ++ [ ("::", 2), ("[]", 0) ])
    ]
  where
    patch mkPatch name =
        mkPatch name (Var.builtin name)

    patternPatch (name, args) =
        Pattern name (Var.builtin name, args)

    tupleNames =
        map fst tuples

    tuples =
        map toTuple [0..9]

    toTuple :: Int -> (String, Int)
    toTuple n =
        ("_Tuple" ++ show n, n)


-- TO TYPE DEALIASER

toDealiaser :: Environment -> Map.Map String String
toDealiaser (Env _ _ _ adts aliases _) =
  let
    dealiasAdt (localName, canonicalSet) =
      case Set.toList canonicalSet of
        [canonicalName] ->
            Just (Var.toString canonicalName, localName)
        _ ->
            Nothing

    dealiasAlias (localName, canonicalSet) =
      case Set.toList canonicalSet of
        [(canonicalName,_,_)] ->
            Just (Var.toString canonicalName, localName)
        _ ->
            Nothing

    adtPairs =
      Maybe.mapMaybe dealiasAdt (Map.toList adts)

    aliasPairs =
      Maybe.mapMaybe dealiasAlias (Map.toList aliases)

    add (key,value) dict =
      Map.insertWith (\v v' -> if length v < length v' then v else v') key value dict
  in
    adtPairs ++ aliasPairs
      |> foldr add Map.empty
