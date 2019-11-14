module ExprTypes where

-- These types are expressions in our lambda calculus.
type Var = String

data Lambda =
  Var :=> Expr
  deriving (Show, Eq)

data Expr =
  Ref Var
  | Lam Lambda
  | Expr :@ Expr
  deriving (Show, Eq)

-- Utility methods.

-- Collect each expression in the AST. It would be nice to have Expr implement Traversable, but that
-- doesn't appear to be possible since Traversable is for types of kind * -> *.
allExpressions :: Expr -> [Expr]
allExpressions expr = case expr of
  f :@ e -> [expr] ++ (allExpressions f) ++ (allExpressions e)
  Lam (x :=> e) -> [expr] ++ (allExpressions e)
  Ref _ -> [expr]
