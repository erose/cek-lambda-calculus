{-# LANGUAGE OverloadedStrings #-}
module Compiler where

-- TODO: printf may cause exceptions to be thrown at runtime. Can I do this safer?
import Text.Printf
import Data.Text (Text, replace, pack, unpack)
import qualified Data.Text.IO
import qualified Data.List

import ExprTypes

-- We're not going to try to be more structured than this.
type PythonCode = String

compileWithTemplate :: Expr -> Text -> Text
compileWithTemplate rootExpr template = replace "{{rootExprReference}}" rootExprText $
  replace "{{exprs}}" exprsText template

  where
    rootExprText = pack $ toPythonFunctionReference rootExpr
    exprsText = pack $ unlines $ map toPythonFunction allUniqueExpressions

  -- The same expression may occur multiple times in the expression tree; if so, there's no need to
  -- produce multiple Python functions for it.
    allUniqueExpressions = Data.List.nub (allExpressions rootExpr)

-- The relevant case of the step function for reference.
-- step (Ref v, ρ, κ) -- Evaluating a reference? Look it up in the environment.
--   = (Lam lam, ρ', κ) where
--     Closure lam ρ' = ρ Map.! v -- Assume the key is in the map.
toPythonFunction :: Expr -> PythonCode
toPythonFunction expr@(Ref v) = printf s (toPythonFunctionReference expr) v
  where s = "\
\def %s():\n\
\  global environment, continuation\n\
\  ((_, _, lam), new_environment) = environment[\"%s\"]\n\
\  \n\
\  environment = new_environment.copy()\n\
\  return lam()\n"

-- The relevant case of the step function for reference.
-- step (f :@ e, ρ, κ) -- Evaluating a function application? First, evaluate the function.
--   = (f, ρ, Ar e ρ κ)
toPythonFunction expr@(f :@ e) = printf s (toPythonFunctionReference expr) (toPythonFunctionReference e) (toPythonFunctionReference f)
  where s = "\
\def %s():\n\
\  global environment, continuation\n\
\  continuation.append((\"Ar\", %s, environment.copy()))\n\
\  return %s()\n"

-- The relevant cases of the step function for reference.
-- step (Lam lam, ρ, Ar e ρ' κ) -- Evaluated the function? Good, now go evaluate the argument term.
--   = (e, ρ', Fn lam ρ κ)
-- -- Evaluated the argument too? Perform the application, now with the argument bound to its value in
-- -- the environment.
-- step (Lam lam, ρ, Fn (x :=> e) ρ' κ)
--   = (e, ρ'', κ) where
--     ρ'' = Map.insert x (Closure lam ρ) ρ'
toPythonFunction expr@(Lam lam) = printf s (toPythonFunctionReference expr) (toPythonFunctionReference expr) (toPythonTuple expr) (toPythonTuple expr)
  where s = "\
\def %s():\n\
\  global environment, continuation\n\
\  if not continuation: return %s # We have terminated.\n\
\  \n\
\  (continuation_type, expression_or_lambda, env_prime) = continuation.pop()\n\
\  if continuation_type == \"Ar\":\n\
\    continuation.append((\"Fn\", %s, environment.copy()))\n\
\    \n\
\    environment = env_prime.copy()\n\
\    return expression_or_lambda()\n\
\  elif continuation_type == \"Fn\":\n\
\    x, e, _ = expression_or_lambda\n\
\    \n\
\    closure = (%s, environment.copy())\n\
\    environment = env_prime.copy()\n\
\    environment[x] = closure\n\
\    return e()\n"

-- Ensure that a string is safe to be used as a substring of a valid Python identifier. A valid
-- Python 3 identifier is described here:
-- https://docs.python.org/3/reference/lexical_analysis.html#identifiers. Since paretheneses occur
-- in expressions but are not allowed in Python identifiers, I use 'l' to stand in for '(' and 'j'
-- to stand in for ')'.

-- e.g. pythonIdentifierSafe (Lam ("x" :=> Lam ("y" :=> Ref "x"))) == "Lam_l_x_to_lam_l_y_to_Ref_xjj"
pythonIdentifierSafe :: String -> String
pythonIdentifierSafe s = unpack $
  replace " " "_" $
  replace ":=>" "to" $
  replace ":@" "apply" $
  replace "(" "l" $
  replace ")" "j" $
  pack s

toPythonFunctionReference :: Expr -> String
toPythonFunctionReference expr = unpack $
  -- Remove the quotes that Show inserts.
  replace "\"" "" $
  pack $
  (++) "expr__" $
  pythonIdentifierSafe (show expr)

-- We choose a weird three-element tuple representation because, in the resulting Python code, we
-- need access to both a) the bound variable as a string, b) the body as a function, and c) the
-- whole thing as a function.
toPythonTuple :: Expr -> String
toPythonTuple expr@(Lam (x :=> e)) =
  "(" ++ (show x) ++ ", " ++ (toPythonFunctionReference e) ++ ", " ++ (toPythonFunctionReference expr) ++ ")"

-- Just for testing.
main :: IO ()
main = do
  -- putStr $ compile $ (Lam ("x" :=> (Ref "x"))) :@ Lam ("y" :=> (Ref "y"))

  let fexpr = Lam ("x" :=> Lam ("y" :=> Ref "x"))
  let doubleexpr = Lam ("x" :=> ((Ref "x") :@ (Ref "x")))

  template <- Data.Text.IO.readFile "template.py"
  putStr $ unpack $ compileWithTemplate (doubleexpr :@ fexpr) template
