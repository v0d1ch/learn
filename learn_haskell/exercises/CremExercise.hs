{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Learn Crem without Hydra, cryptography, or Program.
--
-- From the learn_haskell directory, in your usual development shell:
--   cabal repl exe:crem
-- This uses the project's installed Crem dependency without building Hydra.
-- Cabal exposes the correct dependencies for this exercise.
-- In GHCi:
--   :main transitions
--   :main machine
--   :main runner
--   :main
-- After saving an edit: :reload
--
-- The TODOs deliberately throw at runtime so the unfinished module can load.
-- Use Identity first: it lets us study Crem without instruction interpretation.
module Main where

import Prelude

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM, unless)
import Crem.BaseMachine (ActionResult, BaseMachineT (..), InitialState (..), Transition (..), pureResult, runActionResult)
import Crem.StateMachine (StateMachineT (Executable))
import Crem.StateMachine qualified as Crem
import Crem.Topology (Topology (..))
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- A tiny parcel machine: load a parcel, dispatch it, reset for another parcel.
-- Phase is a type-level label. It does not hold the parcel itself.
data Phase = Empty | Holding | Delivered

-- These are allowed edges, not instructions to execute automatically.
-- Crem also permits every phase to transition to itself.
-- The promoted lists let us avoid Template Haskell in this first exercise.
type ParcelTopology =
  'Topology
    '[ '( 'Empty, '[ 'Holding])
     , '( 'Holding, '[ 'Delivered])
     , '( 'Delivered, '[ 'Empty])
     ]

-- State stores the data available in each phase.
-- HoldingState 7 :: State 'Holding
-- EmptyState    :: State 'Empty
-- There is no parcel to extract from EmptyState.
data State (phase :: Phase) where
  EmptyState :: State 'Empty
  HoldingState :: Int -> State 'Holding
  DeliveredState :: Int -> State 'Delivered

data Command = Load Int | Dispatch | Reset deriving (Eq, Show)
data Reply = Loaded Int | Sent Int | Ready | Ignored deriving (Eq, Show)

-- Exercise 1: implement the four TODOs.
--
--   Empty + Load n     -> reply Loaded n, move to HoldingState n
--   Holding n + Dispatch -> reply Sent n, move to DeliveredState n
--   Delivered n + Reset  -> reply Ready, move to EmptyState
--   Any other command -> reply Ignored, preserve the current state
--
-- Use:
--   pureResult :: (Applicative m, AllowedTransition topology from to)
--              => output -> state to -> ActionResult m topology state from output
--
-- Each branch refines 'from' by matching a State constructor.
-- Neither 'from' nor 'to' is the result type: Reply is the result sent to callers.
step :: State from -> Command -> ActionResult Identity ParcelTopology State from Reply
step EmptyState (Load parcel) = pureResult (Loaded parcel) (HoldingState parcel)
step (HoldingState parcel) Dispatch = pureResult (Sent parcel) (DeliveredState parcel)
step (DeliveredState parcel) Reset = pureResult Ready EmptyState
step state command = pureResult Ignored state

-- Exercise 2: assemble a machine that starts at EmptyState and uses step.
-- BaseMachineT takes an InitialState and an action function.
-- InitialState packages a state while hiding which phase it belongs to.
parcelMachine :: BaseMachineT Identity ParcelTopology Command Reply
parcelMachine = BaseMachineT (InitialState EmptyState) step

-- Supplied: Executable wraps a base machine without diagram-rendering machinery.
initialMachine :: StateMachineT Identity Command Reply
initialMachine = Executable parcelMachine

-- Exercise 3: process commands in order, keeping the returned machine.
--
--   Crem.run :: Monad m => StateMachineT m input output
--            -> input -> m (output, StateMachineT m input output)
--
-- With Identity, runIdentity unwraps that pair.
-- The second element carries the updated state. Do not restart initialMachine
-- for each command. The empty command list produces an empty reply list.
runCommands :: StateMachineT Identity Command Reply -> [Command] -> [Reply]
runCommands _ [] = []
runCommands machine (command:remaining) =
    let (reply, newMachine) = runIdentity $ Crem.run machine command
    in reply: runCommands newMachine remaining

-- Once finished, try this in GHCi:
--   runCommands initialMachine [Load 7, Dispatch, Reset, Load 9, Dispatch]
--
-- Exercise 4: compiler experiment (no implementation required).
-- Uncomment this declaration and reload. Explain the error, then comment it out.
-- Why does adding ('Empty, ['Delivered]) to the topology make it compile?
-- Does the topology prove that parcel 99 was actually loaded?
--
forbidden :: ActionResult Identity ParcelTopology State 'Empty Reply
forbidden = pureResult (Sent 99) (HoldingState 99)
--
-- Questions to answer before moving on to Program:
-- 1. How do Phase and State differ?
-- 2. Why does running Dispatch against initialMachine twice not deliver a parcel?
-- 3. Why does the fallback branch need no explicit edge in ParcelTopology?
-- 4. Which part decides WHEN a transition happens: the topology or step?
--
-- Hydra connection:
--   this step            ~ Rules.ReqSn's step
--   State 'Holding       ~ ReqSn's Built snapshot
--   Identity             ~ Hydra's Program tx ledger signing
--   returned machine     ~ the continuation used by Workflow.run
-- Unlike this exercise, Hydra's Workflow.run feeds () repeatedly until a phase
-- returns Just (). Our machine waits for a Command on every call to Crem.run.

-- Supplied test support: leave everything below unchanged.
-- StateView is only for assertions; it is not the machine's typed state.
data StateView = EmptyView | HoldingView Int | DeliveredView Int deriving (Eq, Show)

view :: State phase -> StateView
view EmptyState = EmptyView
view (HoldingState parcel) = HoldingView parcel
view (DeliveredState parcel) = DeliveredView parcel

inspectStep :: State phase -> Command -> (Reply, StateView)
inspectStep state command =
  case runIdentity (runActionResult (step state command)) of
    Transition reply nextState -> (reply, view nextState)

tests :: [(String, IO ())]
tests =
  [ ("transitions/load", check (Loaded 7, HoldingView 7) $ inspectStep EmptyState (Load 7))
  , ("transitions/dispatch", check (Sent 7, DeliveredView 7) $ inspectStep (HoldingState 7) Dispatch)
  , ("transitions/reset", check (Ready, EmptyView) $ inspectStep (DeliveredState 7) Reset)
  , ("transitions/empty ignores dispatch", check (Ignored, EmptyView) $ inspectStep EmptyState Dispatch)
  , ("transitions/empty ignores reset", check (Ignored, EmptyView) $ inspectStep EmptyState Reset)
  , ("transitions/holding keeps its parcel", check (Ignored, HoldingView 7) $ inspectStep (HoldingState 7) (Load 9))
  , ("transitions/holding ignores reset", check (Ignored, HoldingView 7) $ inspectStep (HoldingState 7) Reset)
  , ("transitions/delivered ignores another dispatch", check (Ignored, DeliveredView 7) $ inspectStep (DeliveredState 7) Dispatch)
  , ("transitions/delivered ignores load", check (Ignored, DeliveredView 7) $ inspectStep (DeliveredState 7) (Load 9))
  , ("machine/starts empty", check Ignored $ fst $ runIdentity $ Crem.run initialMachine Dispatch)
  , ("machine/uses the supplied action", check (Loaded 7) $ fst $ runIdentity $ Crem.run initialMachine (Load 7))
  , ("machine/reset starts at empty", check Ignored $ fst $ runIdentity $ Crem.run initialMachine Reset)
  , ("runner/empty input", check [] $ runCommands initialMachine [])
  , ("runner/retains state between commands", check [Loaded 7, Sent 7] $ runCommands initialMachine [Load 7, Dispatch])
  , ("runner/ignored input preserves state", check [Loaded 7, Ignored, Sent 7] $ runCommands initialMachine [Load 7, Load 9, Dispatch])
  , ("runner/two rounds", check [Loaded 7, Sent 7, Ready, Loaded 9, Sent 9] $ runCommands initialMachine [Load 7, Dispatch, Reset, Load 9, Dispatch])
  ]

check :: (Eq a, Show a) => a -> a -> IO ()
check expected actual = do
  equal <- evaluate (expected == actual)
  unless equal $ fail $ "Expected: " ++ show expected ++ "\nActual:   " ++ show actual

main :: IO ()
main = do
  filters <- getArgs
  let selected = filter (\(name, _) -> null filters || any (`isInfixOf` name) filters) tests
  if null selected
    then putStrLn "Use transitions, machine, runner, or no filter." >> exitFailure
    else do
      results <- forM selected $ \(name, test) -> do
        result <- try test :: IO (Either SomeException ())
        case result of
          Right () -> putStrLn ("PASS " ++ name) >> pure True
          Left err -> putStrLn ("FAIL " ++ name ++ "\n  " ++ displayException err) >> pure False
      putStrLn $ show (length (filter id results)) ++ "/" ++ show (length results) ++ " passed"
      unless (and results) exitFailure
