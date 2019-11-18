import subprocess
import unittest

def run(expr: str) -> str:
  output = subprocess.check_output(f"runhaskell Compiler.hs '{expr}' | python", shell=True)
  return output.decode('utf-8').rstrip()

class TestCompiler(unittest.TestCase):
  def test_id_is_id(self):
    self.assertEqual(
      run('x.x'),
      "x.x"
    )

  def test_id_applied_to_id_is_id(self):
    self.assertEqual(
      run('(x.x)(x.x)'),
      "x.x"
    )

  def test_one_plus_one(self):
    self.assertEqual(
      run('(m.n.f.x.mf(nfx))(f.x.fx)(f.x.fx)'), # (plus one one) in Church numerals.
      "f.x.(f(fx))" # Two, in Church numerals.
    )

  def test_skk_equals_i(self):
    self.assertEqual(
      run('(x.y.z.xz(yz))(x.y.x)(x.y.x)'), # skk, where s and k are of the SKI combinators
      "z.z" # i
    )

  def test_sksk_equals_k(self):
    self.assertEqual(
      run('(x.y.z.xz(yz))(x.y.x)(x.y.z.xz(yz))(x.y.x)'), # sksk, where s and k are of the SKI combinators
      "x.y.x" # k
    )

if __name__ == "__main__":
  # Run the Haskell unit tests.
  subprocess.check_call(["runhaskell", "tests/UnitTests.hs"])

  # Run the compiler (end-to-end) tests.
  unittest.main()
