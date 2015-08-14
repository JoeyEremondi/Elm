{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Optimized
    ( Expr, Expr'
    , Def(..)
    , Facts(..), dummyFacts
    , OptPattern
    ) where

import qualified AST.Expression.General as General
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import Reporting.Region as R


type Expr =
  General.Expr R.Region Def Var.Canonical Type.Canonical


type Expr' =
  General.Expr' R.Region Def Var.Canonical Type.Canonical


type OptPattern = Pattern.Pattern R.Region Var.Canonical

data Def
    = Definition Facts OptPattern Expr
    deriving (Show)


data Facts = Facts
    { tailRecursionDetails :: Maybe (String, Int)
    , defIdent :: Int
    }
    deriving (Eq, Ord, Show)


dummyFacts :: Facts
dummyFacts =
    Facts
    { tailRecursionDetails = Nothing
    , defIdent = -1 --error "Should not access uninitialized id"
    }
