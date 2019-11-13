module Test where
import qualified Data.Map.Strict as Map

import ExprTypes
import qualified Main
import qualified Compiler

testEvaluation :: IO ()
testEvaluation = do
  let fexpr = Lam ("x" :=> Lam ("y" :=> Ref "x"))
  let idexpr = Lam ("x" :=> (Ref "x"))

  let result@(resultExpr, _, _) = Main.evaluate $ fexpr :@ idexpr
  print result
  print $ resultExpr == (Lam ("y" :=> Ref "x"))

  let result@(resultExpr, _, _) = Main.evaluate $ (fexpr :@ idexpr) :@ idexpr
  print result
  print $ resultExpr == (Lam ("x" :=> Ref "x"))

  -- Let's try terminating in a value that is nowhere in our input expression.
  let doubleexpr = Lam ("x" :=> ((Ref "x") :@ (Ref "x")))
  let result@(resultExpr, _, _) = Main.evaluate $ doubleexpr :@ fexpr
  print result
  print $ result == (Lam ("y" :=> Ref "x"), Map.fromList [("x", Main.Closure ("x" :=> Lam ("y" :=> Ref "x")) (Map.fromList []))], Main.Mt)

testSerializingExpressionsToPythonFunctions :: IO ()
testSerializingExpressionsToPythonFunctions = do
  let refExpr = Ref "x"
  let applicationExpr = (Ref "x") :@ (Ref "y")

  let result = Compiler.toPythonFunction refExpr
  print result
  print $ result == "def expr__Ref_x():\n  return environment[\"x\"]()\n"

  let result = Compiler.toPythonFunction applicationExpr
  print result
  print $ result == "def expr__Ref_x_apply_Ref_y():\n  continuation.append((\"Ar\", expr__Ref_y, environment.copy()))\n  return expr__Ref_x()\n"

main :: IO ()
main = do
  testEvaluation
  testSerializingExpressionsToPythonFunctions
