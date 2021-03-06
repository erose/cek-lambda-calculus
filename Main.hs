module Main where
import qualified Data.Map.Strict as Map
import ExprTypes
import Debug.Trace

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
--   - How does the CEK machine ensure that, when you substitute things from the environment back
--     into the control at the end, you don't get something that requires more computation?
--   - How do you derive the CEK machine steps?
--      - EXERCISE: Derive them from Christiansen's eval function.
--   - Something about making a clear distinction between expressions and values. This is one of the
--     things that tripped me up initially, not having before taken a class on the lambda calculus.
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
  | Neu Neutral
  deriving (Show, Eq)

-- Neutral values are used when evaluating under a lambda that hasn't been applied yet. They
-- represent terms that don't have values yet. For example, we might want to reduce the body of a
-- lambda with a single argument x. We would do this by evaluating the body in an environment where
-- x is bound to a NeutralVar.
data Neutral =
  NeutralVar Var

  -- Neutral terms accumulate their arguments on the right, because they will later represent lambda
  -- expressions.
  | Neutral ::@ D
  deriving (Show, Eq)

-- Environments bind names to values.
type Env = Map.Map Var D

-- The type of continuations.
data Kont =
  Mt -- the empty continuation
  | Ar Expr Env Kont -- "Evaluate this argument when I am done evaluating the function."
  | Fn Lambda Env Kont -- "Apply this function to the argument I am currently evaluating, when I finish evaluating it."

  -- "Apply this neutral value to the argument I am currently evaluating, when I finish evaluating
  -- it. It will be the value of Expr."
  | NFn Neutral Expr Kont

  -- "Evaluate this subtree as equal to this neutral value, then terminate unless there is more work
  -- to do."
  | N Neutral Expr Kont
  deriving (Show, Eq)

-- Advances the state machine until it hits a final state.
terminal :: (Σ -> Σ) -> (Σ -> Bool) -> Σ -> Σ
terminal step isFinal current =
  if isFinal current
    then current
  else
    terminal step isFinal (step current)

-- Advances the machine forward by one step.
step :: Σ -> Σ
step state@(_ :@ _, ρ, (N neutralValue parent κ')) -- This expression is neutral.
  = --trace ("Case is f :@ e with continuation N\n" ++ (stateToString state) ++ "\n")
  nextState where
    nextState = case κ' of
      -- This expression is being applied to some other expression. Evaluate the argument, preparing
      -- to apply the value to it.
      Ar e' ρ' κ'' ->
        (e', ρ', NFn neutralValue (parent :@ e') κ'')

      -- This expression is the argument of a function. Perform the application, now with the
      -- argument bound to its value in the environment.
      Fn (x :=> e') ρ' κ'' ->
        (e', ρ'', κ'') where
          ρ'' = Map.insert x (Neu neutralValue) ρ'

      -- Applying a neutral value to the neutral value contained in the continuation.
      NFn n parent' κ'' ->
        (parent', ρ, N (n ::@ (Neu neutralValue)) parent' κ'')

step state@(f :@ e, ρ, κ) -- Evaluating a function application? First, evaluate the function.
  = --trace ("Case is f :@ e\n" ++ (stateToString state) ++ "\n")
  (f, ρ, Ar e ρ κ)

step state@(Lam lam, ρ, Ar e ρ' κ) -- Evaluated the function? Good, now go evaluate the argument term.
  = --trace ("Case is Lam lam on Ar\n" ++ (stateToString state) ++ "\n")
  (e, ρ', Fn lam ρ κ)

-- Evaluated the argument too? Perform the application, now with the argument bound to its value in
-- the environment.
step state@(Lam lam, ρ, Fn (x :=> e) ρ' κ)
  = --trace ("Case is Lam lam on Fn\n" ++ (stateToString state) ++ "\n")
  (e, ρ'', κ) where
    ρ'' = Map.insert x (Closure lam ρ) ρ'

-- Applying a neutral value to a lambda. No evaluation happens, instead we glue it on and go
-- upwards.
step state@(Lam lam, ρ, NFn n parent κ)
  = --trace ("Case is Lam lam on NFn\n" ++ (stateToString state) ++ "\n")
  (parent, ρ, N (n ::@ (Closure lam ρ)) parent κ)

step state@(Ref v, ρ, κ) -- Evaluating a reference? Look it up in the environment.
  = case (ρ !* v) of
      Closure lam ρ' ->
        --trace ("Case is Ref v (Lookup was Closure)\n" ++ (stateToString state) ++ "\n")
        (Lam lam, ρ', κ)

      Neu n ->
        case κ of

        -- Discovered that the left side of an application was a neutral value. Evaluate the right
        -- side, preparing to apply the value to it.
        Ar e ρ' κ' -> --trace ("Case is Ref v (Lookup was Neutral, Kont was Ar)\n" ++ (stateToString state) ++ "\n")
          (e, ρ', NFn n ((Ref v) :@ e) κ')

        -- Discovered that the right side of a function application was a neutral value. Perform the
        -- application, now with the argument bound to its value in the environment.
        Fn (x :=> e) ρ' κ' -> --trace ("Case is Ref v (Lookup was Neutral, Kont was Fn)\n" ++ (stateToString state) ++ "\n")
          (e, ρ'', κ') where
            ρ'' = Map.insert x (Neu n) ρ'

        -- Discovered that the right side of a neutral application was a neutral value. Glue the
        -- terms together, reporting to the parent its value.
        NFn n' parent κ' -> --trace ("Case is Ref v (Lookup was Neutral, Kont was NFn)\n" ++ (stateToString state) ++ "\n")
          (parent, ρ, N (n' ::@ (Neu n)) parent κ')

        -- Discovered that this lookup was a neutral value, and we have no more work to do. Put on
        -- the continuation that asserts this is a normal value. (The state will be terminal.)
        Mt -> --trace ("Case is Ref v (Lookup was Neutral, Kont was Mt)\n" ++ (stateToString state) ++ "\n")
          (Ref v, ρ, N n (Ref v) Mt)

-- Decides whether a state is a final state.
isFinal :: Σ -> Bool
isFinal (Lam _, _, Mt) = True -- An unapplied lambda. No work left to do.
isFinal (_, _, (N value _ Mt)) = True -- The current subtree is neutral, with no work left to do, so we have no work left to do.
isFinal _ = False

-- Evaluates an expression, resulting in a value.
evaluate :: Expr -> D
evaluate e = evaluateWithEnv e Map.empty

-- Evaluates an expression in the context of a certain environment, resulting in a value.
evaluateWithEnv :: Expr -> Env -> D
evaluateWithEnv e ρ = let
  initialState = (e, ρ, Mt)
  finalState = terminal step isFinal initialState
  
  in

  case finalState of
    (Lam lam, ρ', Mt)
      -> Closure lam ρ'

    (_, _, N neutralValue _ Mt)
      -> --trace ("Terminated in neutral value " ++ (show neutralValue) ++ " and ρ' " ++ (show ρ'))
         (Neu neutralValue)

reduce :: Expr -> Expr
reduce e = reduceWithEnv e Map.empty -- We start with the empty environment.

reduceWithEnv :: Expr -> Env -> Expr
reduceWithEnv e ρ =
  case (evaluateWithEnv e ρ) of
    -- The result was a closure; we continue by reducing under the lambda.
    Closure (lam@(x :=> body)) ρ' ->
      -- (trace $ (show lam) ++ " " ++ (show ρ') ++ "\n")
      Lam (x :=> (reduceWithEnv body ρ'')) where
        -- Reduce under the lambda by evaluating the body in an environment where the argument is
        -- bound to a neutral variable.
        ρ'' = Map.insert x (Neu (NeutralVar x)) ρ'

    Neu neutralValue ->
      -- (trace $ (show neutralValue) ++ "\n")
      (neutralToExpr neutralValue)

-- Utility functions.

-- Lookup with a nicer-than-default error message.
(!*) :: (Show k, Ord k, Show a) => Map.Map k a -> k -> a
map !* key = case (Map.lookup key map) of
  Just value -> value
  Nothing -> error $ "Could not find key " ++ (show key) ++ " in " ++ (show map)

-- For debugging.
stateToString :: Σ -> String
stateToString (e, ρ, κ) = "State:\n" ++ (show e) ++ "\n" ++ (show ρ) ++ "\n" ++ (show κ) ++ "\n"

valueToExpr :: D -> Expr
valueToExpr (Closure lam _) = Lam lam
valueToExpr (Neu value) = neutralToExpr value

neutralToExpr :: Neutral -> Expr
neutralToExpr (NeutralVar v) = Ref v
neutralToExpr (a ::@ b) = (neutralToExpr a) :@ (valueToExpr b)
