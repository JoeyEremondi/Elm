{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
module AST.Traversals where

import AST.Expression.Canonical as Canonical
import AST.Expression.General
import qualified Reporting.Annotation as A
import Control.Applicative ((<$>), (<*>), Applicative)
import Control.Monad (forM)
import qualified Control.Monad.Identity as MI


{-
Given an expression a monadic action to perform with an expression,
apply the action bottom-up to each sub-expression of this expression,
as well as the expression itself
-}
mapMExpr
  :: (Monad m, Applicative m) 
  => (Canonical.Expr -> m Canonical.Expr)
  -> Canonical.Expr
  -> m Canonical.Expr
mapMExpr fm (A.A ann e) =
  do let
       self = mapMExpr fm
       newExprM =
         case e of
           (Literal _) ->
             return e
         
           (Var _) ->
             return e
         
           (Range start end) ->
             Range <$> self start <*> self end
         
           (ExplicitList subList) ->
             ExplicitList <$> forM subList self
         
           (Binop op arg1 arg2) ->
             (Binop op) <$> self arg1 <*> self arg2
         
           (Lambda pat body) ->
             (Lambda pat) <$> self body
         
           (App fn arg) ->
             App <$> self fn <*> self arg
         
           (MultiIf pairs) ->
             do let (conds, exprs) = unzip pairs
                newConds <- forM conds self
                newExprs <- forM exprs self
                return $ MultiIf $ zip newConds newExprs
           
           (Let defs body) ->
             do newDefs <- forM defs $ mapMDef fm
                newBody <- self body
                return $ Let newDefs newBody
           
           (Case cexp branches) ->
             do let (pats, branchExps) = unzip branches 
                newCexp <- self cexp
                newExps <- forM branchExps self
                return $ Case newCexp $ zip pats newExps
           
           (Data ctor args) ->
             (Data ctor) <$> forM args self
         
           (Access recExp field) ->
             Access <$> self recExp <*> return field
         
           (Remove recExp field) ->
             Remove <$> self recExp <*> return field
         
           (Insert recExp field arg) ->
             Insert <$> self recExp <*> return field <*> self arg
         
           (Modify recExp fieldPairs) ->
             do let (names, vals) = unzip fieldPairs
                newVals <- forM vals self
                newRec <- self recExp
                return $ Modify newRec $ zip names newVals
           
           (Record pairs) ->
             do let (names, vals) = unzip pairs
                newVals <- forM vals self
                return $ Record $ zip names newVals
           
           (Port sub) ->
             Port <$> mapMPort fm sub
         
           (GLShader _ _ _) ->
             return e
         
     newExpr <- newExprM
     fm $ A.A ann newExpr

    
{-
Given an expression and an expression transforming function,
apply the function bottom-up to each sub-expression of this expression,
as well as the expression itself
-}
mapExpr :: (Canonical.Expr -> Canonical.Expr ) -> Canonical.Expr -> Canonical.Expr
mapExpr f e =
  let
    fm x = return $ f x
  in
   MI.runIdentity $ mapMExpr fm e


--Apply an expression transformer to the right-hand side of a definition
mapMDef
  :: (Monad m, Applicative m)
  => (Canonical.Expr -> m Canonical.Expr )
  -> Canonical.Def
  -> m Canonical.Def
mapMDef f (Definition lhs rhs maybeTipe) =
  do newRHS <- mapMExpr f rhs
     return $ Definition lhs newRHS maybeTipe


--Apply an expression transformer to each expression in a port declaration
mapMPort
  :: (Monad m, Applicative m)
  => (Canonical.Expr -> m Canonical.Expr )
  -> PortImpl Canonical.Expr t
  -> m (PortImpl Canonical.Expr t) 
mapMPort f portImpl =
  case portImpl of 
    (In p1 p2) ->
      return $ In p1 p2
      
    (Out p1 p2 p3) -> do
      newExpr <- mapMExpr f p2
      return $ Out p1 newExpr p3
      
    (Task p1 p2 p3) -> do
      newExpr <- mapMExpr f p2
      return $ Task p1 newExpr p3
