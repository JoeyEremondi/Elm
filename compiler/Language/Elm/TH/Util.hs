-----------------------------------------------------------------------------
--
-- Module      :  Language.Elm.TH.Util
-- Copyright   :  Copyright: (c) 2011-2013 Joey Eremondi
-- License     :  BSD3
--
-- Maintainer  :  joey.eremondi@usask.ca
-- Stability   :  experimental
-- Portability :  portable
--
-- |
--
-----------------------------------------------------------------------------
module Language.Elm.TH.Util where

{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiWayIf #-}

import Language.Haskell.TH.Syntax

import Data.Aeson.TH


import qualified SourceSyntax.Module as M
import qualified SourceSyntax.Declaration as D
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Literal as L
import qualified SourceSyntax.Location as Lo
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as T

import Data.List (isPrefixOf)

import Language.Haskell.TH.Desugar.Sweeten
import Language.Haskell.TH.Desugar


--import Parse.Expression (makeFunction)

import Control.Applicative



-- | General error function for unimplemented features
unImplemented s = error $ "Translation of the The following haskell feature is not yet implemented: " ++ s

emitWarning :: String -> Q [a]
emitWarning s = do
  runIO $ putStrLn $ "Warning! Ignoring feature in Haskell source: " ++ s
  return []


-- |Stolen from Parse.Expression so we don't have to change any internal Elm code
makeFunction :: [P.Pattern] -> E.LExpr -> E.LExpr
makeFunction args body@(Lo.L s _) =
  foldr (\arg body' -> Lo.L s $ E.Lambda arg body') body args

-- |Translate a type variable to a name, ignoring its kind
tyVarToName :: TyVarBndr -> Name
tyVarToName (PlainTV n) = n
tyVarToName (KindedTV n _ ) = n

--Abstract out the translation of names to strings
--So that we can modify if need be
--Right now is just a synonym
nameToString :: Name -> String
nameToString name = 
  case nameModule name of
    Nothing -> nameBase name--TODO fancier?
    Just base -> if  "GHC." `isPrefixOf` base
                      then nameBase name
                      else showName name

--TODO do namespace conversion
nameToElmString = nameToString
--------------------------------------------------------------------------
-- |Type helper functions
--  We use these, since String comparison is insufficients:
-- Template Haskell returns GHC.Types.Int instead of Inst

int = [t| Int |]
string = [t| String |]
float = [t| Float |]
bool = [t| Bool |]

isIntType t = do
  tint <- int
  --runIO $ putStrLn $ "Checking if int " ++ (show (t == tint))
  return (t == tint)

isStringType t = do
  tstr <- string
  return (t == tstr)

isFloatType t = do
  tfloat <- float
  return (t == tfloat)

isBoolType t = do
  tbool <- bool
  return (t == tbool)

-- | Helper function to traverse a tree of AppTs and check if a type is a tuple all the way down  
isTupleType (AppT (TupleT _arity) _) = True
isTupleType (AppT t1 t2) = isTupleType t1
isTupleType _ = False

-- | Helper function to linearize the AppT of tuple types
tupleTypeToList (AppT (TupleT _arity) t) = [t]
tupleTypeToList (AppT t1 t2) = tupleTypeToList t1 ++ [t2]