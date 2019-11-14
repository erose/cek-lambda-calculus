module Main where
import qualified Data.Map.Strict as Map
import ExprTypes

-- NOTES
-- interpretation using closures
-- MachineState = (Control, Environment, ContinutationStack)
-- - modify to support numbers
-- - compiler

-- Somehow, the Env and Kont parts of state space will correspond to global variables.

-- QUESTIONS:
--   - How does compiling the lambda calculus work? Wouldn't you just reduce it to a single
--     expression during your compilation, and then you're done? Given that we're compiling at all,
--     why NOT do this?
--       - Answer: Because this is a compiler, you are restricted to take O(input length) time.
--   - How does the CEK machine ever return values that were not present initially? This didn't
--     initially occur to me, perhaps due to lack of familiarity with the lambda calculus, and Matt
--     Might's article doesn't address it.
--       - Answer: All that data is in the environment, you have to do a substitution/normalization
--         procedure to get it out.
--
-- IDEAS:
--   - Let's create a gadget (function), for each top-level expression, which tells you what to do when
--     applying that expression to an argument. The function takes a single argument...
--   - (I went with this one.) We'll do the steps for the step function, but ignore anything that isn't the Control. We'll
--     output code for each step that switches on the environment and the continuation in order to
--     determine 1) what to set the values of environment and continuation to, 2) where to go next.
--       - OBJECTIONS:
--         - How could this approach ever yield a value that isn't one of expressions to start with?
--           Answer: IDK, how does the CEK machine do that? Answer: it can accumulate an unlimited
--           amount of stuff in the environment, which needs to be substituted in when we finish.
--       - GENERALIZATIONS:
--         - Let there be a (map?) machine of type (Σ \times G) that implements an interpreter for
--           some language, in the sense that we feed in symbols on the input tape, let it run, and
--           after it halts read off something about its state and the contents of the map to see
--           what it's computed. I can transform this into a compiler in the above way. (Each state
--           becomes a function, it looks at the contents of the map to determine where to go and
--           maybe modifies those contents.)

-- The type of our state space.
type Σ = (Expr, Env, Kont)

-- The type of values that expressions in our lambda calculus can take.
data D =
  Closure Lambda Env
  deriving (Show, Eq)

-- Environments bind names to values.
type Env = Map.Map Var D

-- The type of continuations.
data Kont =
  Mt -- the empty continuation
  | Ar Expr Env Kont -- the "I hold an argument to evaluate" continuation
  | Fn Lambda Env Kont -- the "I contain an evaluated function, and now I'm evaluating an argument term" continuation
  deriving (Show, Eq)

-- Advances the state machine until it hits a final state.
terminal :: (Σ -> Σ) -> (Σ -> Bool) -> Σ -> Σ
terminal step isFinal current =
  if (isFinal current)
    then current
  else
    terminal step isFinal (step current)

-- Map an expression into a state.
inject :: Expr -> Σ
inject e = (e, emptyEnv, Mt)
  where emptyEnv = Map.empty

-- Advances the machine forward by one step.
step :: Σ -> Σ
step (Ref v, ρ, κ) -- Evaluating a reference? Look it up in the environment.
  = (Lam lam, ρ', κ) where
    Closure lam ρ' = ρ Map.! v -- Assume the key is in the map.

step (f :@ e, ρ, κ) -- Evaluating a function application? First, evaluate the function.
  = (f, ρ, Ar e ρ κ)

step (Lam lam, ρ, Ar e ρ' κ) -- Evaluated the function? Good, now go evaluate the argument term.
  = (e, ρ', Fn lam ρ κ)

-- Evaluated the argument too? Perform the application, now with the argument bound to its value in
-- the environment.
step (Lam lam, ρ, Fn (x :=> e) ρ' κ)
  = (e, ρ'', κ) where
    ρ'' = Map.insert x (Closure lam ρ) ρ'

-- Decides whether a state is a final state.
isFinal :: Σ -> Bool
isFinal (Lam _, _, Mt) = True -- A single value. Nothing to apply, no work to do.
isFinal _ = False

-- Evaluates an expression, resulting in a terminal state.
evaluate :: Expr -> Σ
evaluate expr = terminal step isFinal (inject expr)
