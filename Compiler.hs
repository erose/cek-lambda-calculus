{-# LANGUAGE OverloadedStrings #-}
module Compiler where

-- TODO: printf may cause exceptions to be thrown at runtime. Can I do this safer?
import Text.Printf
import qualified Data.Text
import qualified Data.List

import ExprTypes

-- We're not going to try to be more structured than this.
type PythonCode = String

compile :: Expr -> String
compile rootExpr = unlines $
  [header] ++
  (map toPythonFunction allUniqueExpressions) ++
  [footer rootExpr]

  where
  -- The same expression may occur multiple times in the expression tree; if so, there's no need to
  -- produce multiple Python functions for it.
    allUniqueExpressions = Data.List.nub (allExpressions rootExpr)

-- Collect each expression in the AST. It would be nice to have Expr implement Traversable, but that
-- doesn't appear to be possible since Traversable is for types of kind * -> *.
allExpressions :: Expr -> [Expr]
allExpressions expr = case expr of
  f :@ e -> [expr] ++ (allExpressions f) ++ (allExpressions e)
  Lam (x :=> e) -> [expr] ++ (allExpressions e)
  Ref _ -> [expr]

-- In the header of the file, we declare some global variables.
header :: PythonCode
header = "\
\ # Lambda = (String, Function, Function)\n\
\ # D = (Lambda, Env)\n\
\environment = {} # Of type Env = Dict (String -> D)\n\
\continuation = [] # Of type [String, Function | Lambda, Env]\n"

-- In the footer of the generated file, we have our entry point (calling the function representing
-- the root expression).
footer :: Expr -> PythonCode
footer rootExpr = printf s (toPythonFunctionReference rootExpr)
  where s = "\
\if __name__ == \"__main__\":\n\
\  try:\n\
\    print(%s())\n\
\  except Exception:\n\
\    print(\"environment:\", environment)\n\
\    print(\"continuation:\", continuation)\n\
\    raise\n"

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
-- TODO: Do we want a shallow or deep copy of the environment here? I think shallow.
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
\  if not continuation:\n\
\    # We have terminated.\n\
\    return %s\n\
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

-- TODO: In more generality, we want a way to serialize Exprs and Lambdas into Python literals.

-- Ensure that a string is safe to be used as a substring of a valid Python identifier. A valid
-- Python 3 identifier is described here:
-- https://docs.python.org/3/reference/lexical_analysis.html#identifiers. Since paretheneses occur
-- in expressions but are not allowed in Python identifiers, I use 'l' to stand in for '(' and 'j'
-- to stand in for ')'.

-- e.g. pythonIdentifierSafe (Lam ("x" :=> Lam ("y" :=> Ref "x"))) == "Lam_L_x_to_lam_L_y_to_Ref_xJJ"
pythonIdentifierSafe :: String -> String
pythonIdentifierSafe s = Data.Text.unpack $
  Data.Text.replace " " "_" $
  Data.Text.replace ":=>" "to" $
  Data.Text.replace ":@" "apply" $
  Data.Text.replace "(" "l" $
  Data.Text.replace ")" "j" $
  Data.Text.pack s

toPythonFunctionReference :: Expr -> String
toPythonFunctionReference expr = Data.Text.unpack $
  -- Remove the quotes that Show inserts.
  Data.Text.replace "\"" "" $
  Data.Text.pack $
  (++) "expr__" $
  pythonIdentifierSafe (show expr)

toPythonTuple :: Expr -> String
toPythonTuple expr@(Lam (x :=> e)) =
  "(" ++ (show x) ++ ", " ++ (toPythonFunctionReference e) ++ ", " ++ (toPythonFunctionReference expr) ++ ")"

-- Just for testing.
main :: IO ()
main = do
  putStr $ compile $ (Lam ("x" :=> (Ref "x"))) :@ Lam ("y" :=> (Ref "y"))
  -- putStr $ compile $ (Lam ("x" :=> ((Ref "x") :@ (Ref "x")))) :@ Lam ("x" :=> (Ref "x"))
