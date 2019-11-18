import subprocess
import unittest

def run(expr: str) -> str:
  output = subprocess.check_output(f"runhaskell Compiler.hs '{expr}' | python", shell=True)
  return output.decode('utf-8').rstrip()

class TestCompiler(unittest.TestCase):
  def test_id_is_id(self):
    self.assertEqual(
      run('x.x'),
      "Lam x :=> (Ref x)"
    )

  def test_id_applied_to_id_is_id(self):
    self.assertEqual(
      run('(x.x)(x.x)'),
      "Lam x :=> (Ref x)"
    )

  def test_one_plus_one(self):
    self.assertEqual(
      "Lam f :=> (Lam x :=> ((Ref f) :@ ((Ref f) :@ (Ref x))))", # Two, in Church numerals.
      run('(m.n.f.x.mf(nfx))(f.x.fx)(f.x.fx)') # (plus one one) in Church numerals.
    )

if __name__ == "__main__":
  # Run the Haskell unit tests.
  subprocess.check_call(["runhaskell", "tests/UnitTests.hs"])

  # Run the compiler (end-to-end) tests.
  unittest.main()
