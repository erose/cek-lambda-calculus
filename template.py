from abc import ABC
from typing import *

class Expr(ABC):
  tag: str
  def __eq__(self, other):
    return str(self) == str(other)

#####

class Ref(Expr):
  def __init__(self, var: str):
    self.tag = "Ref"
    self.var = var

  def __repr__(self):
    return f'{self.var}'

class Lambda(Expr):
  def __init__(self, x: str, e: Expr):
    self.tag = "Lambda"
    self.x = x
    self.e = e

  def __repr__(self):
    return f'{self.x}.{self.e}'

class Application(Expr):
  def __init__(self, f: Expr, e: Expr):
    self.tag = "Application"
    self.f = f
    self.e = e

  def __repr__(self):
    return f'({self.f}{self.e})'

# A value.
class D(ABC):
  tag: str

Env = Dict[str, D]

class Closure(D):
  def __init__(self, lam: Lambda, env: Env):
    self.tag = "Closure"
    self.lam = lam
    self.env = env

  def __repr__(self):
    return f'Closure({self.lam}, {self.env})'

class Neutral(D, ABC):
  neutral_type: str

  def __init__(self):
    self.tag = "Neutral"

class NeutralVar(Neutral):
  def __init__(self, var: str):
    self.neutral_type = "NeutralVar"
    self.var = var
    super().__init__()

  def __repr__(self):
    return f'NeutralVar {self.var}'

class NeutralApplication(Neutral):
  def __init__(self, left: Neutral, right: D):
    self.neutral_type = "NeutralApplication"
    self.left = left
    self.right = right
    super().__init__()

  def __repr__(self):
    return f'NeutralApplication ({self.left}) ({self.right})'

#####

class Continuation(ABC):
  tag: str
  def __init__(self, previous: 'Continuation'): # Forward reference in mypy.
    self.previous = previous

  def __repr__(self):
    if self.previous is None:
      return f'{self.tag}'
    return f'{self.tag} ({self.previous})'

class Mt(Continuation):
  def __init__(self):
    self.tag = "Mt"
    super().__init__(None) # Mt has no previous continuation.

class Ar(Continuation):
  def __init__(self, e: Expr, env: Env, previous: Continuation):
    self.tag = "Ar"
    self.e = e
    self.env = env
    super().__init__(previous)

class Fn(Continuation):
  def __init__(self, lam: Lambda, env: Env, previous: Continuation):
    self.tag = "Fn"
    self.lam = lam
    self.env = env
    super().__init__(previous)

class NFn(Continuation):
  def __init__(self, neutral: Neutral, expr: Expr, previous: Continuation):
    self.tag = "NFn"
    self.neutral = neutral
    self.parent = expr
    super().__init__(previous)

class N(Continuation):
  def __init__(self, neutral: Neutral, expr: Expr, previous: Continuation):
    self.tag = "N"
    self.neutral = neutral
    self.parent = expr
    super().__init__(previous)

#####

# Check when we should stop computing, based on our state.
# 
# Haskell implementation, for reference.
# isFinal (Lam _, _, Mt) = True -- An unapplied lambda. No work left to do.
# isFinal (_, _, (N value _ Mt)) = True -- The current subtree is neutral, with no work left to do, so we have no work left to do.
# isFinal _ = False
def is_final():
  global expr
  return (
    (expr.tag == "Lambda" and kont.tag == 'Mt') or
    (kont.tag == 'N' and kont.previous and kont.previous.tag == 'Mt')
  )

# Utility method.
def set_new_state(e: Expr, env: Env, k: Continuation):
  global expr, environment, kont
  expr = e
  environment = env
  kont = k

# Haskell implementations for reference.
# valueToExpr :: D -> Expr
# valueToExpr (Closure lam _) = Lam lam
# valueToExpr (Neu value) = neutralToExpr value

# neutralToExpr :: Neutral -> Expr
# neutralToExpr (NeutralVar v) = Ref v
# neutralToExpr (a ::@ b) = (neutralToExpr a) :@ (valueToExpr b)
def value_to_expr(value: D) -> Expr:
  if value.tag == 'Closure':
    closure = cast(Closure, value)
    return closure.lam

  if value.tag == 'Neutral':
    value = cast(Neutral, value)
    return neutral_to_expr(value)

  raise Exception('Error')

def neutral_to_expr(neutral: Neutral) -> Expr:
  if neutral.neutral_type == "NeutralVar":
    neutral = cast(NeutralVar, neutral)
    return Ref(neutral.var)

  if neutral.neutral_type == "NeutralApplication":
    neutral = cast(NeutralApplication, neutral)
    return Application(
      neutral_to_expr(neutral.left),
      value_to_expr(neutral.right)
    )

  raise Exception('Error')

# Haskell implementation, for reference.
# evaluateWithEnv :: Expr -> Env -> D
# evaluateWithEnv e ρ = let
#   initialState = (e, ρ, Mt)
#   finalState = terminal step isFinal initialState
  
#   in

#   case finalState of
#     (Lam lam, ρ', Mt)
#       -> Closure lam ρ'

#     (_, _, N neutralValue _ Mt)
#       -> --trace ("Terminated in neutral value " ++ (show neutralValue) ++ " and ρ' " ++ (show ρ'))
#          (Neu neutralValue)
def evaluate() -> D:
  global expr, environment, kont

  while True:
    # print(expr, environment, kont) # For debugging.
{{switchOnExpr}}

  # If we finished on a lambda expression.
  if expr.tag == "Lambda":
    lam = cast(Lambda, expr)
    return Closure(lam, environment.copy())

  # If we finished with a neutral value.
  if kont.tag == 'N' and kont.previous and kont.previous.tag == 'Mt':
    return cast(N, kont).neutral

  raise Exception('Error')

# Haskell implementation, for reference.
# reduceWithEnv e ρ =
#   case (evaluateWithEnv e ρ) of
#     -- The result was a closure; we continue by reducing under the lambda.
#     Closure (lam@(x :=> body)) ρ' ->
#       Lam (x :=> (reduceWithEnv body ρ'')) where
#         -- Reduce under the lambda by evaluating the body in an environment where the argument is
#         -- bound to a neutral variable.
#         ρ'' = Map.insert x (Neu (NeutralVar x)) ρ'

#     Neu neutralValue ->
#       neutralToExpr neutralValue
def reduce() -> Expr:
  global expr

  value = evaluate()
  # print(value) # For debugging.
  if value.tag == 'Closure':
    closure = cast(Closure, value)
    environment[closure.lam.x] = NeutralVar(closure.lam.x)

    expr = closure.lam.e
    return Lambda(closure.lam.x, reduce())

  if value.tag == "Neutral":
    return neutral_to_expr(cast(Neutral, value))

  raise Exception('Error')

#####

# Declare global variables.
expr = None
expr = cast(Optional[Expr], expr)
environment = {} # type: ignore
environment = cast(Env, environment)
kont = cast(Continuation, Mt())

# Define the expressions.
{{exprDefinitions}}


# This is our entry point.
if __name__ == "__main__":
  try:
    expr = {{rootExpr}}
    print(reduce())
  except Exception:
    # For debugging.
    print("environment:", environment)
    print("kont:", kont)
    raise
