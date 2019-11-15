module Test where
import qualified Data.Map.Strict as Map

import ExprTypes
import qualified Main
import qualified Compiler

testEvaluationToCEKState :: IO ()
testEvaluationToCEKState = do
  let fexpr = Lam ("x" :=> Lam ("y" :=> Ref "x"))
  let idexpr = Lam ("x" :=> (Ref "x"))

  let (result, empty, Main.Mt) = Main.evaluateToCEKState $ fexpr :@ idexpr
  assertEqual (Lam ("y" :=> Ref "x")) result

  let (result, empty, Main.Mt) = Main.evaluateToCEKState $ (fexpr :@ idexpr) :@ idexpr
  assertEqual (Lam ("x" :=> Ref "x")) result

  let doubleexpr = Lam ("x" :=> ((Ref "x") :@ (Ref "x")))
  assertEqual (Lam ("y" :=> Ref "x"), Map.fromList [("x", Main.Closure ("x" :=> Lam ("y" :=> Ref "x")) (Map.fromList []))], Main.Mt)
              (Main.evaluateToCEKState $ doubleexpr :@ fexpr)

testEvaluation :: IO ()
testEvaluation = do
  -- Test basic evaluation with neutral variables.
  let qx_x = ((Ref "q") :@ (Lam ("x" :=> (Ref "x"))))
  let envWhereQIsNeutral = Map.fromList [("q", Main.Neu (Main.NeutralVar "q"))]

  -- q(x.x) = q(x.x)
  assertEqual qx_x $ Main.evaluateWithEnv qx_x envWhereQIsNeutral

  let x_x = Lam ("x" :=> (Ref "x"))
  let qk = (Ref "q") :@ (Ref "k")
  let envWhereQAndKAreNeutral = Map.fromList [("q", Main.Neu (Main.NeutralVar "q")), ("k", Main.Neu (Main.NeutralVar "k"))]
  let qkx_x = qk :@ x_x

  -- qk(x.x) = qk(x.x)
  assertEqual qkx_x $ Main.evaluateWithEnv qkx_x envWhereQAndKAreNeutral

  -- (x.x)(qk) = qk
  assertEqual qk $ Main.evaluateWithEnv (x_x :@ qk) envWhereQAndKAreNeutral

  let qkr = qk :@ (Ref "r")
  let envWhereQAndKAndRAreNeutral = Map.fromList [("q", Main.Neu (Main.NeutralVar "q")), ("k", Main.Neu (Main.NeutralVar "k")), ("r", Main.Neu (Main.NeutralVar "r"))]

  -- qkr = qkr
  assertEqual qkr $ Main.evaluateWithEnv qkr envWhereQAndKAndRAreNeutral

  -- Test that various SKI combinator identities hold.
  let s = Lam ("x" :=> Lam ("y" :=> Lam ("z" :=> (((Ref "x") :@ (Ref "z")) :@ ((Ref "y") :@ (Ref "z"))))))
  let k = Lam ("x" :=> Lam ("y" :=> (Ref "x")))
  let i = Lam ("x" :=> (Ref "x"))

  -- skk = i
  assertEqual (Lam ("z" :=> (Ref "z"))) (Main.evaluate ((s:@k):@k))

  -- sksk = k
  assertEqual k (Main.evaluate (((s:@k):@s):@k))

-- Commented out for now as these functions have not proved to be stable.
-- testSerializingExpressionsToPythonFunctions :: IO ()
-- testSerializingExpressionsToPythonFunctions = do
--   let refExpr = Ref "x"
--   let applicationExpr = (Ref "x") :@ (Ref "y")

--   assertEqual "def expr__Ref_x():\n  global environment, continuation\n  l, new_environment = environment[\"x\"]\n  \n  old_environment = environment\n  environment = new_environment\n  result = l()\n  environment = old_environment\n  \n  return result\n"
--               (Compiler.toPythonFunction refExpr)

--   assertEqual "def expr__Ref_x_apply_Ref_y():\n  continuation.append((\"Ar\", expr__Ref_y, environment.copy()))\n  return expr__Ref_x()\n"
--               (Compiler.toPythonFunction applicationExpr)

-- Utilities.
assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual expected actual = do
  if (expected == actual) then do
    return ()
  else do
    putStrLn ("Expected: " ++ show expected)
    putStrLn ("Actual: " ++ show actual)
    error "Assertion failed"

main :: IO ()
main = do
  testEvaluation
  testEvaluationToCEKState
  -- testSerializingExpressionsToPythonFunctions
