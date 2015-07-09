{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
module AST.Traversals where

import AST.Expression.Canonical as Canonical
import AST.Expression.General
import qualified Reporting.Annotation as A
import qualified Data.List as List


{-
Given an expression and an expression transforming function,
apply the function bottom-up to each sub-expression of this expression,
as well as the expression itself
-}
mapExpr :: (Canonical.Expr -> Canonical.Expr ) -> Canonical.Expr -> Canonical.Expr
mapExpr f (A.A ann e) =
  f $ A.A ann newExpr
  where
    self = mapExpr f
    newExpr =
      case e of
        (Literal _sub) -> e
        (Var _sub) -> e
        (Range sub1 sub2) -> Range (self sub1) (self sub2) 
        (ExplicitList subs) -> ExplicitList (List.map self subs)
        (Binop sub1 sub2 sub3) -> Binop sub1 (self sub2) (self sub3)
        (Lambda sub1 sub2) -> Lambda sub1 (self sub2)
        (App sub1 sub2) -> App (self sub1) (self sub2)
        (MultiIf subs) -> MultiIf $ List.map (\(x,y) -> (self x, self y) ) subs
        (Let defs body) -> Let (List.map (mapDef f) defs ) (self body)
        (Case cexp branches) -> Case (self cexp) (map (\(p,ex) -> (p, self ex )) branches)
        (Data sub1 sub2) -> Data sub1 (map self sub2)
        (Access sub1 sub2) -> Access (self sub1) sub2
        (Remove sub1 sub2) -> Remove (self sub1) sub2
        (Insert sub1 sub2 sub3) -> Insert (self sub1) sub2 (self sub3)
        (Modify sub1 sub2) -> Modify (self sub1) sub2
        (Record subs) -> Record $ map (\(s,ex) -> (s, self ex) ) subs
        (Port sub) -> Port $ mapPort f sub
        (GLShader sub1 sub2 sub3) -> GLShader sub1 sub2 sub3

--Apply an expression transformer to the right-hand side of a definition
mapDef :: (Canonical.Expr -> Canonical.Expr ) -> Canonical.Def -> Canonical.Def
mapDef f (Definition lhs rhs maybeTipe) =
  Definition lhs (mapExpr f rhs) maybeTipe

--Apply an expression transformer to each expression in a port declaration
mapPort :: (Canonical.Expr -> Canonical.Expr ) -> PortImpl Canonical.Expr t -> PortImpl Canonical.Expr t 
mapPort f portImpl =
  case portImpl of 
    (In p1 p2) ->
      In p1 p2
    (Out p1 p2 p3) ->
      Out p1 (mapExpr f p2) p3
    (Task p1 p2 p3) ->
      Task p1 (mapExpr f p2) p3
