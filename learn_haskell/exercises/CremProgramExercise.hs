{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Connect your Program and Crem exercises.
--
-- From the learn_haskell directory, in your usual development shell:
--   cabal repl exe:crem-program
-- Cabal exposes the correct dependencies for this exercise.
-- In GHCi:
--   :main program
--   :main interpreter
--   :main machine
--   :main
-- Save edits, then :reload. Each test has a one-second timeout.
--
-- Only this file is needed; your earlier exercises remain separate.
-- The instances, machine wiring and runner are supplied. Implement the TODOs.
module Main where

import Prelude

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM, unless)
import Crem.BaseMachine (ActionResult (ActionResult), BaseMachineT (BaseMachineT), InitialState (InitialState), Transition (Transition), pureResult, runActionResult)
import Crem.StateMachine (StateMachineT (Executable))
import Crem.StateMachine qualified as Crem
import Crem.Topology (Topology (Topology))
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Timeout (timeout)

-- Program describes requests; it does not supply their meaning.
-- This is your previous representation without Hydra's capability parameters.
newtype Program a = Program
  { interpretProgram :: forall r. (a -> r) -> (forall x. Instruction x -> (x -> r) -> r) -> r
  }

data Instruction a where
  MakeLabel :: Int -> Instruction String
  Record :: String -> Instruction ()

instance Functor Program where
  fmap f (Program program) = Program $ \done handle -> program (done . f) handle

instance Applicative Program where
  pure a = Program $ \done _ -> done a
  Program functions <*> Program arguments = Program $ \done handle ->
    functions (\f -> arguments (done . f) handle) handle

instance Monad Program where
  Program program >>= next = Program $ \done handle ->
    program (\a -> interpretProgram (next a) done handle) handle

instruction :: Instruction a -> Program a
instruction op = Program $ \done handle -> handle op done

makeLabel :: Int -> Program String
makeLabel = instruction . MakeLabel

record :: String -> Program ()
record = instruction . Record

-- Exercise 1: describe the work using makeLabel and record.
-- Request a label for the parcel, record "label: " ++ label, then return label.
-- Do not call either label provider here: the interpreter chooses the provider.
labelParcel :: Int -> Program String
labelParcel parcel = error "TODO 1: request, record, return"

-- Exercise 2: give those requests meaning.
-- The interpreter supplies both completion and instruction handlers.
-- The log starts empty and uses reverse order internally for cheap prepending.
-- Return it in chronological order when the program completes.
--
-- Notice r here is [String] -> (a, [String]). An instruction's next function
-- accepts its reply FIRST, then the current log. Calling next continues the
-- program; it does not call a Crem state transition directly.
runProgram :: forall a. (Int -> String) -> Program a -> (a, [String])
runProgram provideLabel (Program program) = program done handleInstruction []
 where
  done :: a -> [String] -> (a, [String])
  done value reversedLog = error "TODO 2a: finish with value and chronological log"

  handleInstruction :: forall x. Instruction x -> (x -> [String] -> (a, [String])) -> [String] -> (a, [String])
  handleInstruction (MakeLabel parcel) next reversedLog =
    error "TODO 2b: obtain the label and pass it to next; keep the log"
  handleInstruction (Record message) next reversedLog =
    error "TODO 2c: add the message and continue with the unit reply"

-- Two meanings for MakeLabel, without changing the program or the machine.
shippingLabels :: Int -> String
shippingLabels parcel = "SHIP-" ++ show parcel

testLabels :: Int -> String
testLabels parcel = "TEST-" ++ show parcel

-- Same topology as the previous exercise. Delivered now also retains a label.
data Phase = Empty | Holding | Delivered

type ParcelTopology =
  'Topology
    '[ '( 'Empty, '[ 'Holding])
     , '( 'Holding, '[ 'Delivered])
     , '( 'Delivered, '[ 'Empty])
     ]

data State (phase :: Phase) where
  EmptyState :: State 'Empty
  HoldingState :: Int -> State 'Holding
  DeliveredState :: Int -> String -> State 'Delivered

data Command = Load Int | Dispatch | Reset deriving (Eq, Show)
data Reply = Loaded Int | Sent Int String | Ready | Ignored deriving (Eq, Show)

-- Exercise 3: connect the program to a Crem transition.
-- Use labelParcel to obtain the label. Return Sent parcel label together with
-- DeliveredState parcel label. The do block runs in Program, not IO.
--
-- This function chooses work for a STATE + COMMAND.
-- handleInstruction above handles an INSTRUCTION + CONTINUATION.
-- We deliberately give them different names rather than calling both "step".
transition :: State from -> Command -> ActionResult Program ParcelTopology State from Reply
transition EmptyState (Load parcel) = pureResult (Loaded parcel) (HoldingState parcel)
transition (HoldingState parcel) Dispatch = ActionResult $ do
  (error "TODO 3: request work and return the reply and destination" :: Program (Reply, State 'Delivered))
transition (DeliveredState _ _) Reset = pureResult Ready EmptyState
transition state _ = pureResult Ignored state

initialMachine :: StateMachineT Program Command Reply
initialMachine = Executable $ BaseMachineT (InitialState EmptyState) transition

-- Supplied: same recursion you wrote before, now in Program rather than Identity.
-- Crem.run returns a Program containing a reply and the updated machine.
runCommands :: StateMachineT Program Command Reply -> [Command] -> Program [Reply]
runCommands _ [] = pure []
runCommands machine (command : commands) = do
  (reply, nextMachine) <- Crem.run machine command
  replies <- runCommands nextMachine commands
  pure (reply : replies)

-- Building this value requests no external IO and chooses no label provider.
delivery :: Program [Reply]
delivery = runCommands initialMachine [Load 7, Dispatch, Reset, Load 9, Dispatch]

-- After completing the TODOs, compare:
--   runProgram shippingLabels delivery
--   runProgram testLabels delivery
--
-- Questions:
-- 1. Where does the String returned by makeLabel actually come from?
-- 2. Which next function receives it, and where does that continuation lead?
-- 3. Why does changing the label provider require no topology changes?
-- 4. What would happen if the Record handler called itself with the same inputs
--    instead of calling next?
-- 5. Why do Load and ignored commands still work with pureResult?
--
-- Hydra connection: MakeLabel plays the role of a primitive such as SignSnapshot.
-- Our interpreter computes a label and collects messages; it does no real IO.

-- Supplied tests and inspection support: leave unchanged.
-- This view exposes instruction order without solving the logging interpreter.
data Requests a
  = Finished a
  | NeedsLabel Int (String -> Requests a)
  | Recorded String (Requests a)

inspect :: Program a -> Requests a
inspect (Program program) = program Finished handle
 where
  handle :: Instruction x -> (x -> Requests a) -> Requests a
  handle (MakeLabel parcel) next = NeedsLabel parcel next
  handle (Record message) next = Recorded message (next ())

labelRequestMatches :: Int -> String -> Bool
labelRequestMatches parcel supplied = case inspect (labelParcel parcel) of
  NeedsLabel requested next ->
    requested == parcel && case next supplied of
      Recorded message (Finished result) -> message == "label: " ++ supplied && result == supplied
      _ -> False
  _ -> False

data StateView = EmptyView | HoldingView Int | DeliveredView Int String deriving (Eq, Show)

view :: State phase -> StateView
view EmptyState = EmptyView
view (HoldingState parcel) = HoldingView parcel
view (DeliveredState parcel label) = DeliveredView parcel label

inspectTransition :: State phase -> Command -> Program (Reply, StateView)
inspectTransition state command = do
  Transition reply nextState <- runActionResult (transition state command)
  pure (reply, view nextState)

tests :: [(String, IO ())]
tests =
  [ ("program/requests a label before recording", check True $ labelRequestMatches 7 "chosen-label")
  , ("program/uses the supplied reply", check True $ labelRequestMatches 9 "different-label")
  , ("interpreter/completion", check (42 :: Int, []) $ runProgram shippingLabels (pure 42))
  , ("interpreter/label reply", check ("SHIP-7", []) $ runProgram shippingLabels (makeLabel 7))
  , ("interpreter/alternative provider", check ("TEST-7", []) $ runProgram testLabels (makeLabel 7))
  , ("interpreter/record unit reply", check ((), ["hello"]) $ runProgram shippingLabels (record "hello"))
  , ("interpreter/chronological log", check ((), ["first", "second"]) $ runProgram shippingLabels (record "first" >> record "second"))
  , ("interpreter/continues after record", check ("TEST-9", ["before"]) $ runProgram testLabels (record "before" >> makeLabel 9))
  , ("machine/dispatch retains the label", check ((Sent 7 "SHIP-7", DeliveredView 7 "SHIP-7"), ["label: SHIP-7"]) $ runProgram shippingLabels (inspectTransition (HoldingState 7) Dispatch))
  , ("machine/alternative interpreter reply", check ((Sent 7 "TEST-7", DeliveredView 7 "TEST-7"), ["label: TEST-7"]) $ runProgram testLabels (inspectTransition (HoldingState 7) Dispatch))
  , ("machine/empty dispatch requests no label", check ([Ignored], []) $ runProgram (error "provider called") (runCommands initialMachine [Dispatch]))
  , ("machine/ignored load preserves the parcel", check ([Loaded 7, Ignored, Sent 7 "TEST-7"], ["label: TEST-7"]) $ runProgram testLabels (runCommands initialMachine [Load 7, Load 9, Dispatch]))
  , ("machine/does not dispatch twice", check ([Loaded 7, Sent 7 "SHIP-7", Ignored], ["label: SHIP-7"]) $ runProgram shippingLabels (runCommands initialMachine [Load 7, Dispatch, Dispatch]))
  , ("machine/two rounds", check ([Loaded 7, Sent 7 "SHIP-7", Ready, Loaded 9, Sent 9 "SHIP-9"], ["label: SHIP-7", "label: SHIP-9"]) $ runProgram shippingLabels delivery)
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
    then putStrLn "Use program, interpreter, machine, or no filter." >> exitFailure
    else do
      results <- forM selected $ \(name, test) -> do
        result <- try (timeout 1000000 test) :: IO (Either SomeException (Maybe ()))
        case result of
          Right (Just ()) -> putStrLn ("PASS " ++ name) >> pure True
          Right Nothing -> putStrLn ("TIMEOUT " ++ name ++ " (one second)") >> pure False
          Left err -> putStrLn ("FAIL " ++ name ++ "\n  " ++ displayException err) >> pure False
      putStrLn $ show (length (filter id results)) ++ "/" ++ show (length results) ++ " passed"
      unless (and results) exitFailure
