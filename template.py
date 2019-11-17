from abc import ABC
from typing import *

# An Expr is just a reference to a function.
class Expr:
  def __init__(self, function):
    self.function = function

#####

# A Lambda is an Expr, but with a reference to the argument and the body subexpression.
class Lambda(Expr):
  def __init__(self, var: str, body: Expr, *args):
    self.var = var
    self.body = body
    super().__init__(*args)

# A value.
class D(ABC):
  pass

Env = Dict[str, D]

class Closure(D):
  def __init__(self, lam: Lambda, env: Env):
    self.lam = lam
    self.env = env

class Neutral(ABC):
  pass

class NeutralVar(Neutral):
  def __init__(self, var: str):
    self.var = var

class NeutralApplication(Neutral):
  def __init__(self, left: Neutral, right: D):
    self.left = left
    self.right = right

#####

class Continuation(ABC):
  pass

class Mt(Continuation):
  def __init__(self):
    self.tag = "Mt"

class Ar(Continuation):
  def __init__(self, expr: Expr, env: Env):
    self.tag = "Ar"
    self.expr = expr
    self.env = env

class Fn(Continuation):
  def __init__(self, lam: Lambda, env: Env):
    self.tag = "Fn"
    self.lam = lam
    self.env = env

class NFn(Continuation):
  def __init__(self, neutral: Neutral, expr: Expr):
    self.tag = "NFn"
    self.neutral = neutral
    self.parent = expr

class N(Continuation):
  def __init__(self, neutral: Neutral, expr: Expr):
    self.tag = "N"
    self.neutral = neutral
    self.parent = expr

#####

# Declare global variables.
  
environment: Env = {}
continuations: List[Continuation] = []

{{exprs}}

# This is our entry point.
if __name__ == "__main__":
  try:
    term = {{rootExprReference}}
    print(term(), environment)
  except Exception:
    # For debugging.
    print("environment:", environment)
    print("continuations:", continuations)
    raise
