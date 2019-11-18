{-# LANGUAGE OverloadedStrings #-}
module Compiler where

-- TODO: printf may cause exceptions to be thrown at runtime. Can I do this safer?
import Text.Printf
import Data.Text (Text, replace, pack, unpack)
import System.Environment (getArgs)
import qualified Data.Text.IO
import qualified Data.List

import ExprTypes
import Parser (parseExpr)

-- We're not going to try to be more structured than this.
type PythonCode = String

compileWithTemplate :: Expr -> Text -> Text
compileWithTemplate rootExpr template = replace "{{rootExpr}}" rootExprText $
  replace "{{exprDefinitions}}" exprsDefinitionsText $
  replace "{{switchOnExpr}}" switchOnExprText $
  template

  where
    rootExprText = pack $ toPythonReference rootExpr
    exprsDefinitionsText = pack $ unlines $ map toPythonDefinition allUniqueExpressions
    switchOnExprText = pack $ unlines $ map toSwitchCase allUniqueExpressions

  -- The same expression may occur multiple times in the expression tree; if so, there's no need to
  -- produce multiple Python functions for it.
    allUniqueExpressions = Data.List.nub (allExpressions rootExpr)

toPythonDefinition :: Expr -> PythonCode
toPythonDefinition expr = (toPythonReference expr) ++ " = " ++ (toPythonValue expr)

toPythonValue :: Expr -> PythonCode
toPythonValue (Lam (x :=> e)) = "Lambda(" ++ (show x) ++ ", " ++ (toPythonValue e) ++ ")"
toPythonValue (f :@ e) = "Application(" ++ (toPythonValue f) ++ ", " ++ (toPythonValue e) ++ ")"
toPythonValue (Ref v) = "Ref(" ++ (show v) ++ ")"

-- The relevant case of the step function, for reference.
-- step state@(Ref v, ρ, κ) -- Evaluating a reference? Look it up in the environment.
--   = case (ρ !* v) of
--       Closure lam ρ' ->
--         --trace ("Case is Ref v (Lookup was Closure)\n" ++ (stateToString state) ++ "\n")
--         (Lam lam, ρ', κ)

--       Neu n ->
--         case κ of

--         -- Discovered that the left side of an application was a neutral value. Evaluate the right
--         -- side, preparing to apply the value to it.
--         Ar e ρ' κ' -> --trace ("Case is Ref v (Lookup was Neutral, Kont was Ar)\n" ++ (stateToString state) ++ "\n")
--           (e, ρ', NFn n ((Ref v) :@ e) κ')

--         -- Discovered that the right side of a function application was a neutral value. Perform the
--         -- application, now with the argument bound to its value in the environment.
--         Fn (x :=> e) ρ' κ' -> --trace ("Case is Ref v (Lookup was Neutral, Kont was Fn)\n" ++ (stateToString state) ++ "\n")
--           (e, ρ'', κ') where
--             ρ'' = Map.insert x (Neu n) ρ'

--         -- Discovered that the right side of a neutral application was a neutral value. Glue the
--         -- terms together, reporting to the parent its value.
--         NFn n' parent κ' -> --trace ("Case is Ref v (Lookup was Neutral, Kont was NFn)\n" ++ (stateToString state) ++ "\n")
--           (parent, ρ, N (n' ::@ (Neu n)) parent κ')

--         -- Discovered that this lookup was a neutral value, and we have no more work to do. Put on
--         -- the continuation that asserts this is a normal value. (The state will be terminal.)
--         Mt -> --trace ("Case is Ref v (Lookup was Neutral, Kont was Mt)\n" ++ (stateToString state) ++ "\n")
--           (Ref v, ρ, N n (Ref v) Mt)
toSwitchCase :: Expr -> PythonCode
toSwitchCase expr@(Ref v) = printf s (toPythonReference expr) v (toPythonReference expr) (toPythonReference expr) (toPythonReference expr)
  where s = "\
\    if expr == %s:\n\
\      expr = cast(Ref, expr)\n\
\      if is_final(): break\n\
\      \n\
\      value = environment['%s']\n\
\      if value.tag == 'Closure':\n\
\        closure = cast(Closure, value)\n\
\        \n\
\        environment = closure.env.copy()\n\
\        expr = closure.lam\n\
\        # continuation is unchanged\n\
\        continue\n\
\      \n\
\      if value.tag != 'Neutral': raise Exception('Error')\n\
\      n = cast(Neutral, value)\n\
\      \n\
\      if continuation.tag == 'Ar':\n\
\        continuation = cast(Ar, continuation)\n\
\        \n\
\        environment = continuation.env.copy()\n\
\        expr = continuation.e\n\
\        continuation = NFn(n, Application(%s, continuation.e), continuation.previous)\n\
\        continue\n\
\      if continuation.tag == 'Fn':\n\
\        continuation = cast(Fn, continuation)\n\
\        \n\
\        environment = continuation.env.copy()\n\
\        environment[continuation.lam.x] = n\n\
\        expr = continuation.lam.e\n\
\        continuation = continuation.previous\n\
\        continue\n\
\      if continuation.tag == 'NFn':\n\
\        continuation = cast(NFn, continuation)\n\
\        \n\
\        # environment is unchanged\n\
\        expr = continuation.parent\n\
\        continuation = N(NeutralApplication(continuation.neutral, n), continuation.parent, continuation.previous)\n\
\        continue\n\
\      if continuation.tag == 'Mt':\n\
\        # environment is unchanged\n\
\        expr = %s\n\
\        continuation = N(n, %s, Mt())\n\
\        continue\n"

-- The relevant case of the step function for reference.
-- step state@(_ :@ _, ρ, (N neutralValue parent κ')) -- This expression is neutral.
--   = --trace ("Case is f :@ e with continuation N\n" ++ (stateToString state) ++ "\n")
--   nextState where
--     nextState = case κ' of
--       -- This expression is being applied to some other expression. Evaluate the argument, preparing
--       -- to apply the value to it.
--       Ar e' ρ' κ'' ->
--         (e', ρ', NFn neutralValue (parent :@ e') κ'')

--       -- This expression is the argument of a function. Perform the application, now with the
--       -- argument bound to its value in the environment.
--       Fn (x :=> e') ρ' κ'' ->
--         (e', ρ'', κ'') where
--           ρ'' = Map.insert x (Neu neutralValue) ρ'

--       -- Applying a neutral value to the neutral value contained in the continuation.
--       NFn n parent' κ'' ->
--         (parent', ρ, N (n ::@ (Neu neutralValue)) parent' κ'')

-- step state@(f :@ e, ρ, κ) -- Evaluating a function application? First, evaluate the function.
--   = --trace ("Case is f :@ e\n" ++ (stateToString state) ++ "\n")
--   (f, ρ, Ar e ρ κ)
toSwitchCase expr@(f :@ e) = printf s (toPythonReference expr) (toPythonReference e) (toPythonReference f)
  where s = "\
\    if expr == %s:\n\
\      expr = cast(Application, expr)\n\
\      if is_final(): break\n\
\      \n\
\      if continuation.tag == 'N':\n\
\        continuation = cast(N, continuation)\n\
\        previous_continuation = continuation.previous\n\
\        \n\
\        if previous_continuation.tag == 'Ar':\n\
\          previous_continuation = cast(Ar, previous_continuation)\n\
\          \n\
\          # environment is unchanged\n\
\          expr = previous_continuation.e\n\
\          continuation = NFn(continuation.neutral, Application(continuation.parent, previous_continuation.e), previous_continuation.previous)\n\
\          continue\n\
\        if previous_continuation.tag == 'Fn':\n\
\          previous_continuation = cast(Fn, previous_continuation)\n\
\          \n\
\          environment = previous_continuation.env.copy()\n\
\          environment[previous_continuation.lam.x] = continuation.neutral\n\
\          expr = previous_continuation.lam.e\n\
\          continuation = previous_continuation.previous\n\
\          continue\n\
\        if previous_continuation.tag == 'NFn':\n\
\          previous_continuation = cast(NFn, previous_continuation)\n\
\          \n\
\          # environment is unchanged\n\
\          expr = previous_continuation.parent\n\
\          continuation = N(NeutralApplication(continuation.neutral, previous_continuation.neutral), previous_continuation.parent, previous_continuation.previous)\n\
\          continue\n\
\      else:\n\
\        # environment is unchanged\n\
\        expr = %s\n\
\        continuation = Ar(%s, environment.copy(), continuation)\n\
\        continue\n\
\\n"

-- The relevant case of the step function for reference.
-- step state@(Lam lam, ρ, Ar e ρ' κ) -- Evaluated the function? Good, now go evaluate the argument term.
--   = --trace ("Case is Lam lam on Ar\n" ++ (stateToString state) ++ "\n")
--   (e, ρ', Fn lam ρ κ)

-- -- Evaluated the argument too? Perform the application, now with the argument bound to its value in
-- -- the environment.
-- step state@(Lam lam, ρ, Fn (x :=> e) ρ' κ)
--   = --trace ("Case is Lam lam on Fn\n" ++ (stateToString state) ++ "\n")
--   (e, ρ'', κ) where
--     ρ'' = Map.insert x (Closure lam ρ) ρ'

-- -- Applying a neutral value to a lambda. No evaluation happens, instead we glue it on and go
-- -- upwards.
-- step state@(Lam lam, ρ, NFn n parent κ)
--   = --trace ("Case is Lam lam on NFn\n" ++ (stateToString state) ++ "\n")
--   (parent, ρ, N (n ::@ (Closure lam ρ)) parent κ)
toSwitchCase expr@(Lam lam) = printf s (toPythonReference expr)
  where s = "\
\    if expr == %s:\n\
\      expr = cast(Lambda, expr)\n\
\      if is_final(): break\n\
\      \n\
\      if continuation.tag == 'Ar':\n\
\        continuation = cast(Ar, continuation)\n\
\        \n\
\        environment = continuation.env.copy()\n\
\        old_expr = expr\n\
\        expr = continuation.e\n\
\        continuation = Fn(old_expr, environment.copy(), continuation.previous)\n\
\        continue\n\
\      if continuation.tag == 'Fn':\n\
\        continuation = cast(Fn, continuation)\n\
\        \n\
\        old_environment = environment.copy()\n\
\        environment = continuation.env.copy()\n\
\        environment[continuation.lam.x] = Closure(expr, old_environment)\n\
\        expr = continuation.lam.e\n\
\        continuation = continuation.previous\n\
\        continue\n\
\      if continuation.tag == 'NFn':\n\
\        continuation = cast(NFn, continuation)\n\
\        \n\
\        # environment is unchanged\n\
\        neutral_app = NeutralApplication(continuation.neutral, Closure(expr, environment.copy()))\n\
\        expr = continuation.parent\n\
\        continuation = N(neutral_app, continuation.parent, continuation.previous)\n\
\        continue\n\
\\n"

toPythonReference :: Expr -> String
toPythonReference expr = unpack $
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
  "(" ++ (show x) ++ ", " ++ (toPythonReference e) ++ ", " ++ (toPythonReference expr) ++ ")"

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

-- Just for testing.
main :: IO ()
main = do
  -- let idexpr = Lam ("x" :=> (Ref "x"))
  -- let fexpr = Lam ("x" :=> Lam ("y" :=> Ref "x"))
  -- let doubleexpr = Lam ("x" :=> ((Ref "x") :@ (Ref "x")))

  -- Some bindings that are available to use.
  let s = Lam ("x" :=> Lam ("y" :=> Lam ("z" :=> (((Ref "x") :@ (Ref "z")) :@ ((Ref "y") :@ (Ref "z"))))))
  let k = Lam ("x" :=> Lam ("y" :=> (Ref "x")))
  let i = Lam ("x" :=> (Ref "x"))

  args <- getArgs
  let input = args !! 0
  case (parseExpr input) of
    Left error -> do
      putStrLn (show error)

    Right expr -> do
      template <- Data.Text.IO.readFile "template.py"
      putStr $ unpack $ compileWithTemplate expr template
