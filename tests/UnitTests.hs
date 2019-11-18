module UnitTests where
import qualified Data.Map.Strict as Map

import ExprTypes
import qualified Main
import qualified Compiler

testEvaluation :: IO ()
testEvaluation = do
  let fexpr = Lam ("x" :=> Lam ("y" :=> Ref "x"))
  let idexpr = Lam ("x" :=> (Ref "x"))

  assertEqual (Main.Closure ("x" :=> Ref "x") Map.empty) $ Main.evaluate ((fexpr:@idexpr):@idexpr)
  assertEqual (Main.Closure ("y" :=> Ref "x") (Map.fromList [("x", Main.Closure ("x" :=> Ref "x") (Map.fromList []))]))
    (Main.evaluate (fexpr:@idexpr))

  let doubleexpr = Lam ("x" :=> ((Ref "x") :@ (Ref "x")))
  assertEqual (Main.Closure ("y" :=> Ref "x") (Map.fromList [("x", Main.Closure ("x" :=> Lam ("y" :=> Ref "x")) (Map.fromList []))]))
              (Main.evaluate $ doubleexpr :@ fexpr)

testReduction :: IO ()
testReduction = do
  -- Test basic evaluation with neutral variables.
  let q = Ref "q"
  let qx_x = (q :@ (Lam ("x" :=> (Ref "x"))))
  let envWhereQIsNeutral = Map.fromList [("q", Main.Neu (Main.NeutralVar "q"))]

  -- q(x.x) = q(x.x)
  assertEqual qx_x $ Main.reduceWithEnv qx_x envWhereQIsNeutral

  let r = Ref "r"
  let x_x = Lam ("x" :=> (Ref "x"))
  let qr = q :@ r
  let envWhereQAndRAreNeutral = Map.fromList [("q", Main.Neu (Main.NeutralVar "q")), ("r", Main.Neu (Main.NeutralVar "r"))]
  let qrx_x = qr :@ x_x

  -- qr(x.x) = qr(x.x)
  assertEqual qrx_x $ Main.reduceWithEnv qrx_x envWhereQAndRAreNeutral

  -- (x.x)(qr) = qr
  assertEqual qr $ Main.reduceWithEnv (x_x :@ qr) envWhereQAndRAreNeutral

  let t = Ref "t"
  let qrt = qr :@ t
  let envWhereQAndRAndTAreNeutral = Map.fromList [("q", Main.Neu (Main.NeutralVar "q")), ("r", Main.Neu (Main.NeutralVar "r")), ("t", Main.Neu (Main.NeutralVar "t"))]

  -- qrt = qrt
  assertEqual qrt $ Main.reduceWithEnv qrt envWhereQAndRAndTAreNeutral

  let doubleexpr = Lam ("x" :=> ((Ref "x") :@ (Ref "x")))
  -- Show that we are able to produce expressions ocurring nowhere in the input.
  -- (x.xx)(q) = qq.
  assertEqual (q :@ q) $ Main.reduceWithEnv (doubleexpr :@ q) envWhereQIsNeutral

  -- Tests involving the SKI combinators.
  let s = Lam ("x" :=> Lam ("y" :=> Lam ("z" :=> (((Ref "x") :@ (Ref "z")) :@ ((Ref "y") :@ (Ref "z"))))))
  let k = Lam ("x" :=> Lam ("y" :=> (Ref "x")))
  let i = Lam ("x" :=> (Ref "x"))

  -- sqrt = (qt)(rt)
  assertEqual ((q:@t):@(r:@t)) $ Main.reduceWithEnv (((s:@q):@r):@t) envWhereQAndRAndTAreNeutral

  -- skk = i
  -- TODO: Implement alpha-equality for this.
  assertEqual (Lam ("z" :=> (Ref "z"))) $ Main.reduce ((s:@k):@k)

  -- sksk = k
  assertEqual k (Main.reduce (((s:@k):@s):@k))

  -- s(k(si))k is a combinator that reverses the following two terms
  -- s(k(si))kqr = rq
  let reversal = (s:@(k:@(s:@i))):@k
  assertEqual (r:@q) $ Main.reduceWithEnv ((reversal:@q):@r) envWhereQAndRAreNeutral

testChurchNumeralReduction :: IO ()
testChurchNumeralReduction = do
  let zero = Lam ("f" :=> Lam ("x" :=> (Ref "x")))
  let one = Lam ("f" :=> Lam ("x" :=> ((Ref "f") :@ (Ref "x"))))
  let two = Lam ("f" :=> Lam ("x" :=> ((Ref "f") :@ ((Ref "f") :@ (Ref "x")))))
  let three = Lam ("f" :=> Lam ("x" :=> ((Ref "f") :@ ((Ref "f") :@ ((Ref "f") :@ (Ref "x"))))))
  let plus = Lam ("m" :=> Lam ("n" :=> Lam ("f" :=> Lam ("x" :=> ( ((Ref "m") :@ (Ref "f")) :@ (((Ref "n") :@ (Ref "f")) :@ (Ref "x")) )))))

  -- 0 + 0 == 0
  assertEqual zero $ Main.reduce ((plus :@ zero) :@ zero)

  -- 0 + 1 == 1
  assertEqual one $ Main.reduce ((plus :@ zero) :@ one)

  -- 1 + 1 == 2
  assertEqual two $ Main.reduce ((plus :@ one) :@ one)

  -- 1 + 2 == 3
  assertEqual three $ Main.reduce ((plus :@ one) :@ two)

  -- 2 + 1 == 3
  assertEqual three $ Main.reduce ((plus :@ two) :@ one)

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
  testReduction
  testChurchNumeralReduction
  -- testSerializingExpressionsToPythonFunctions
