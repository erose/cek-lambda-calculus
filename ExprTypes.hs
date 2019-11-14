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
