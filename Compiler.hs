{-# LANGUAGE OverloadedStrings #-}
module Compiler where

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
toSwitchCase expr@(Ref v) = printf s (toPythonReference expr)
  where s = "\
\    if expr == %s:\n\
\      expr = cast(Ref, expr)\n\
\      if is_final(): break\n\
\      \n\
\      value = environment[expr.var]\n\
\      if value.tag == 'Closure':\n\
\        closure = cast(Closure, value)\n\
\        \n\
\        set_new_state(closure.lam, closure.env.copy(), kont)\n\
\        continue\n\
\      \n\
\      if value.tag != 'Neutral': raise Exception('Error')\n\
\      n = cast(Neutral, value)\n\
\      \n\
\      if kont.tag == 'Ar':\n\
\        kont = cast(Ar, kont)\n\
\        \n\
\        new_kont = NFn(n, Application(expr, kont.e), kont.previous)\n\
\        set_new_state(kont.e, kont.env.copy(), new_kont)\n\
\        continue\n\
\      if kont.tag == 'Fn':\n\
\        kont = cast(Fn, kont)\n\
\        \n\
\        new_environment = kont.env.copy()\n\
\        new_environment[kont.lam.x] = n\n\
\        set_new_state(kont.lam.e, new_environment, kont.previous)\n\
\        continue\n\
\      if kont.tag == 'NFn':\n\
\        kont = cast(NFn, kont)\n\
\        \n\
\        new_kont = N(NeutralApplication(kont.neutral, n), kont.parent, kont.previous)\n\
\        set_new_state(kont.parent, environment, new_kont)\n\
\        continue\n\
\      if kont.tag == 'Mt':\n\
\        set_new_state(expr, environment, N(n, expr, Mt()))\n\
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
toSwitchCase expr@(f :@ e) = printf s (toPythonReference expr)
  where s = "\
\    if expr == %s:\n\
\      expr = cast(Application, expr)\n\
\      if is_final(): break\n\
\      \n\
\      if kont.tag == 'N':\n\
\        kont = cast(N, kont)\n\
\        previous_kont = kont.previous\n\
\        \n\
\        if previous_kont.tag == 'Ar':\n\
\          previous_kont = cast(Ar, previous_kont)\n\
\          \n\
\          new_kont = NFn(kont.neutral, Application(kont.parent, previous_kont.e), previous_kont.previous)\n\
\          set_new_state(previous_kont.e, previous_kont.env.copy(), new_kont)\n\
\          continue\n\
\        if previous_kont.tag == 'Fn':\n\
\          previous_kont = cast(Fn, previous_kont)\n\
\          \n\
\          new_environment = previous_kont.env.copy()\n\
\          new_environment[previous_kont.lam.x] = kont.neutral\n\
\          set_new_state(previous_kont.lam.e, new_environment, previous_kont.previous)\n\
\          continue\n\
\        if previous_kont.tag == 'NFn':\n\
\          previous_kont = cast(NFn, previous_kont)\n\
\          \n\
\          new_kont = N(NeutralApplication(previous_kont.neutral, kont.neutral), previous_kont.parent, previous_kont.previous)\n\
\          set_new_state(previous_kont.parent, environment, new_kont)\n\
\          continue\n\
\      else:\n\
\        new_kont = Ar(expr.e, environment.copy(), kont)\n\
\        set_new_state(expr.f, environment, new_kont)\n\
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
\      if kont.tag == 'Ar':\n\
\        kont = cast(Ar, kont)\n\
\        \n\
\        set_new_state(kont.e, kont.env.copy(), Fn(expr, environment.copy(), kont.previous))\n\
\        continue\n\
\      if kont.tag == 'Fn':\n\
\        kont = cast(Fn, kont)\n\
\        \n\
\        new_environment = kont.env.copy()\n\
\        new_environment[kont.lam.x] = Closure(expr, environment.copy())\n\
\        set_new_state(kont.lam.e, new_environment, kont.previous)\n\
\        continue\n\
\      if kont.tag == 'NFn':\n\
\        kont = cast(NFn, kont)\n\
\        \n\
\        neutral_app = NeutralApplication(kont.neutral, Closure(expr, environment.copy()))\n\
\        set_new_state(kont.parent, environment, N(neutral_app, kont.parent, kont.previous))\n\
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
