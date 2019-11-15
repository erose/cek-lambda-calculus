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
  let qr = (Ref "q") :@ (Ref "r")
  let envWhereQAndRAreNeutral = Map.fromList [("q", Main.Neu (Main.NeutralVar "q")), ("r", Main.Neu (Main.NeutralVar "r"))]
  let qrx_x = qr :@ x_x

  -- qr(x.x) = qr(x.x)
  assertEqual qrx_x $ Main.evaluateWithEnv qrx_x envWhereQAndRAreNeutral

  -- (x.x)(qr) = qr
  assertEqual qr $ Main.evaluateWithEnv (x_x :@ qr) envWhereQAndRAreNeutral

  let qrt = qr :@ (Ref "t")
  let envWhereQAndRAndTAreNeutral = Map.fromList [("q", Main.Neu (Main.NeutralVar "q")), ("r", Main.Neu (Main.NeutralVar "r")), ("t", Main.Neu (Main.NeutralVar "t"))]

  -- qrt = qrt
  assertEqual qrt $ Main.evaluateWithEnv qrt envWhereQAndRAndTAreNeutral

  -- Tests involving the SKI combinators.
  let s = Lam ("x" :=> Lam ("y" :=> Lam ("z" :=> (((Ref "x") :@ (Ref "z")) :@ ((Ref "y") :@ (Ref "z"))))))
  let k = Lam ("x" :=> Lam ("y" :=> (Ref "x")))
  let i = Lam ("x" :=> (Ref "x"))

  -- sqrt = (qt)(rt)
  -- TODO: Implement alpha-equality for this.
  assertEqual (((Ref "x"):@(Ref "z")):@((Ref "y"):@(Ref "z"))) $ Main.evaluateWithEnv (((s:@(Ref "q")):@(Ref "r")):@(Ref "t")) envWhereQAndRAndTAreNeutral

  -- skk = i
  -- TODO: Implement alpha-equality for this.
  assertEqual (Lam ("z" :=> (Ref "z"))) $ Main.evaluate ((s:@k):@k)

  -- sksk = k
  assertEqual k (Main.evaluate (((s:@k):@s):@k))

  -- s(k(si))k is a combinator that reverses the following two terms
  -- s(k(si))kqr = rq
  -- let reversal = (s:@(k:@(s:@i))):@k
  -- assertEqual ((Ref "r") :@ (Ref "q")) $ Main.evaluateWithEnv ((reversal:@(Ref "q")):@(Ref "r")) envWhereQAndRAreNeutral

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
