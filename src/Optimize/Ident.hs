module Optimize.Ident  where

import qualified AST.Traversals as ASTT
import qualified AST.Expression.Canonical as Canon
import qualified Reporting.Annotation as A

import qualified Control.Monad.State as State

addUniqueIds :: Canon.Expr -> Canon.Expr
addUniqueIds ex =
  let
    applyId (A.A ann e) =
      do
        currentId <- State.get
        State.put (currentId + 1)
        return $ A.A (ann {A.ident = currentId}) e
  in State.evalState (ASTT.mapMExpr applyId ex) 1
