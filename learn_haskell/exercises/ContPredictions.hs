-- | Predict each result before evaluating it in GHCi.
-- Uses your instances from ContExercise.hs.
--
-- From the learn_haskell directory:
--   cabal repl exe:cont
-- In GHCi:
--   :module *ContPredictions
--   exercise1
-- Save edits, then use :reload.
--
-- For each exercise, fill in the prediction and reasoning comments first.
-- Explain who calls each continuation and what happens to its result.
module ContPredictions where

import Debug.Trace
import Main (Cont (..))

-- 1. Passing a value onward.
-- Prediction: 12
-- Reasoning:
exercise1 :: Int
exercise1 = runCont (pure 4) (\x -> x * 3)

-- 2. Mapping before the final continuation.
-- Prediction: 18
-- Reasoning:
exercise2 :: Int
exercise2 = runCont (fmap (+ 2) (pure 4)) (\x -> x * 3)

-- 3. Doing something with the continuation's result.
-- Compare with exercise 2: where does the addition happen?
-- Prediction: 14
-- Reasoning:
exercise3 :: Int
exercise3 =
  let computation = Cont $ \k -> k 4 + 2
   in runCont computation (\x -> x * 3)

-- 4. Ignoring the continuation.
-- Does either (+ 1) or (* 3) get used?
-- Prediction: 303 -> 100
-- Reasoning: + 1 gets used
exercise4 :: Int
exercise4 =
  let computation = do
        x <- Cont $ \_k -> 100
        pure (x + 1)
   in runCont computation (\x -> x * 3)

-- 5. Calling the continuation twice.
-- What exactly is repeated when k is called twice?
-- Prediction: 91 -> 72
-- Reasoning:
exercise5 :: Int
exercise5 =
  let computation = do
        x <- Cont $ \k -> k 2 + k 5
        pure (x * 10)
   in runCont computation (\x -> x + 1)

-- (x * 10) (\n -> n + 1) -> x * 10 + 1
-- 2 * 10 + 1 + 5 = 26
exercise5a :: Int
exercise5a =
  let computation = do
        x <- Cont $ \k -> k 2 + 5
        pure (x * 10)
   in runCont computation (\x -> x + 1)

-- 6. Observing Applicative order.
-- Use the instance that runs the function computation first.
-- Which arithmetic happens inside each continuation call, and which happens
-- after it returns?
-- Prediction: 130
-- Reasoning:
exercise6 :: Int
exercise6 =
  let functions = Cont $ \k -> k (+ 1) + 100
      values = Cont $ \k -> k 2 * 10
   in runCont (functions <*> values) id
