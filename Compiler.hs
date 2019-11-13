{-# LANGUAGE OverloadedStrings #-}
module Compiler where
import Text.Printf
import qualified Data.Text

import ExprTypes

-- We will emit Python code as a string.
type PythonFunction = String

compile :: Expr -> String
-- TODO: Need more boilerplate, including an entry point.
compile = unlines . expressionToFunctions

-- Walk the AST and spit out a Python function for each expression.
-- TODO: Traversable instance?
expressionToFunctions :: Expr -> [PythonFunction]
expressionToFunctions expr = case expr of
  f :@ e -> (expressionToFunctions f) ++ (expressionToFunctions e)
  Lam (x :=> e) -> [toLamFunction expr] ++ (expressionToFunctions e)
  Ref _ -> [toPythonFunction expr]

toPythonFunction :: Expr -> PythonFunction
toPythonFunction expr = case expr of
  Ref _ -> toRefFunction expr
  _ :@ _ -> toApplicationFunction expr
  Lam _ -> toLamFunction expr

-- TODO: printf may cause exceptions to be thrown at runtime. Can I do this safer?

-- The relevant case of the step function for reference.
-- step (Ref v, ρ, κ) -- Evaluating a reference? Look it up in the environment.
--   = (Lam lam, ρ', κ) where
--     Closure lam ρ' = ρ Map.! v -- Assume the key is in the map.
toRefFunction :: Expr -> PythonFunction
toRefFunction expr@(Ref v) = printf s (toPythonIdentifier expr) v
  where s = "\
\def expr__%s():\n\
\  return environment[\"%s\"]()\n"

-- The relevant case of the step function for reference.
-- step (f :@ e, ρ, κ) -- Evaluating a function application? First, evaluate the function.
--   = (f, ρ, Ar e ρ κ)
toApplicationFunction :: Expr -> PythonFunction
-- TODO: Do we want a shallow or deep copy of the environment here? I think shallow.
toApplicationFunction expr@(f :@ e) = printf s (toPythonIdentifier expr) (toPythonIdentifier e) (toPythonIdentifier f)
  where s = "\
\def expr__%s():\n\
\  continuation.append((\"Ar\", expr__%s, environment.copy()))\n\
\  return expr__%s()\n"

-- The relevant cases of the step function for reference.
-- step (Lam lam, ρ, Ar e ρ' κ) -- Evaluated the function? Good, now go evaluate the argument term.
--   = (e, ρ', Fn lam ρ κ)
-- -- Evaluated the argument too? Perform the application, now with the argument bound to its value in
-- -- the environment.
-- step (Lam lam, ρ, Fn (x :=> e) ρ' κ)
--   = (e, ρ'', κ) where
--     ρ'' = Map.insert x (Closure lam ρ) ρ'
toLamFunction :: Expr -> PythonFunction
-- TODO: How to handle Closure?
toLamFunction expr@(Lam lam) = printf s (toPythonIdentifier expr) (toPythonIdentifier expr) (toPythonTuple lam) (toPythonTuple lam)
  where s = "\
\def expr__%s():\n\
\  if not continuation:\n\
\    # We have terminated.\n\
\    return expr__%s\n\
\  \n\
\  (continuation_type, expression_or_lambda, env_prime) = continuation.pop()\n\
\  if continuation_type == \"Ar\":\n\
\    expression = expression_or_lambda\n\
\    continuation.append((\"Fn\", %s, environment.copy()))\n\
\    return expression()\n\
\  elif continuation_type == \"Fn\":\n\
\    l = expression_or_lambda\n\
\    x, e = l\n\
\    environment[x] = %s # TODO: This is suspect\n\
\    return e()\n"

-- TODO: In more generality, we want a way to serialize Exprs and Lambdas into Python literals.
-- Serialize an expression into a string that is safe to be used as a substring of a valid Python
-- identifier. A valid Python 3 identifier is described here:
-- https://docs.python.org/3/reference/lexical_analysis.html#identifiers. Since paretheneses occur
-- in expressions but are not allowed in Python identifiers, I use 'l' to stand in for '('
-- and 'j' to stand in for ')'.

-- e.g. toPythonIdentifier (Lam ("x" :=> Lam ("y" :=> Ref "x"))) == "Lam_L_x_to_lam_L_y_to_Ref_xJJ"
toPythonIdentifier :: Expr -> String
toPythonIdentifier expr = Data.Text.unpack $
  Data.Text.replace " " "_" $
  Data.Text.replace ":=>" "to" $
  Data.Text.replace ":@" "apply" $
  Data.Text.replace "(" "l" $
  Data.Text.replace ")" "j" $
  Data.Text.replace "\"" "" $ -- TODO: Is this really necessary?
  Data.Text.pack $ show expr

toPythonTuple :: Lambda -> String
toPythonTuple (x :=> expr) = "(" ++ (show x) ++ ", " ++ (toPythonIdentifier expr) ++ ")"

-- Just for testing.
main :: IO ()
main = do
  putStr $ compile (Lam ("x" :=> ((Ref "x") :@ (Ref "x"))))
