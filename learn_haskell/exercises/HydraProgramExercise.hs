{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Standalone Hydra Program exercise; only base is needed.
--
-- From the learn_haskell directory:
--   cabal repl exe:hydra-program
-- In GHCi:
--   :main functor
--   :main pure
--   :main applicative
--   :main monad
--   :main
-- Save edits, then use :reload.
--
-- Implement the four TODOs in that order. Implement Applicative without bind.
-- The error placeholders let the module load before the exercise is finished.
-- Replace a placeholder with a typed hole (_) to inspect available types.
-- For more relevant bindings: :set -fno-max-relevant-binds
--
-- Program has Hydra's exact representation and capability parameters.
-- Instruction retains its constructor/result structure, with base-only toy
-- payloads below. The supplied interpreter models staging and termination;
-- its ledger and signature operations are examples, not Cardano implementations.
module Main where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM, unless)
import Data.List (isInfixOf)
import Data.Void (Void, absurd)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- Toy payloads keep the interesting Program and Instruction types visible.
type ChainSlot = Int
type UTxOType tx = [tx]
type HeadId = String
type StateChanged tx = String
type Effect tx = String
type WaitReason tx = String
type LogicError tx = String

newtype ValidationError = ValidationError String deriving (Eq, Show)
newtype Snapshot tx = Snapshot [tx] deriving (Eq, Show)
newtype Signature a = Signature String deriving (Eq, Show)
type MultiSignature a = [Signature a]
data HydraKey
type VerificationKey key = String
data Verified = Verified | Failed deriving (Eq, Show)

newtype Program tx (ledger :: Bool) (signing :: Bool) a = Program
  { interpretProgram ::
      forall r.
      (a -> r) ->
      (forall x. Instruction tx ledger signing x -> (x -> r) -> r) ->
      r
  }

data Instruction tx (ledger :: Bool) (signing :: Bool) a where
  ApplyTransaction :: ChainSlot -> UTxOType tx -> tx -> Instruction tx 'True signing (Either ValidationError (UTxOType tx))
  SignSnapshot :: Snapshot tx -> Instruction tx ledger 'True (Signature (Snapshot tx))
  VerifySnapshot :: [VerificationKey HydraKey] -> MultiSignature (Snapshot tx) -> String -> Instruction tx ledger signing Verified
  RecordAppliedTransaction :: HeadId -> tx -> UTxOType tx -> UTxOType tx -> Instruction tx ledger signing ()
  Change :: StateChanged tx -> Instruction tx ledger signing ()
  Emit :: Effect tx -> Instruction tx ledger signing ()
  Decide :: String -> Bool -> Instruction tx ledger signing Bool
  Suspend :: WaitReason tx -> Instruction tx ledger signing Void
  Reject :: LogicError tx -> Instruction tx ledger signing Void

type TransactionProgram tx = Program tx 'True 'False
type SnapshotProgram tx = Program tx 'True 'True
type AcknowledgementProgram tx = Program tx 'False 'False

-- Exercise 1: adapt the returned value, preserving instruction handling.
instance Functor (Program tx ledger signing) where
  fmap f (Program program) = Program $ \done step -> program (\a -> done (f a)) step

-- Exercise 2: return a value without issuing any instructions.
-- Exercise 3: sequence the function program before the argument program.
instance Applicative (Program tx ledger signing) where
  pure a = Program $ \done step -> done a
  Program functions <*> Program arguments = Program $ \done step -> functions (\f -> arguments (\a -> done (f a)) step) step

-- Exercise 4: the value from the first program chooses the next program.
instance Monad (Program tx ledger signing) where
  Program program >>= next = Program $ \done step -> program (\a -> let Program t = next a in t done step) step

-- Supplied helpers, interpreter and tests: leave these unchanged.
instruction :: Instruction tx ledger signing a -> Program tx ledger signing a
instruction op = Program $ \done step -> step op done

-- Direct fixtures let each instance's tests run independently of the others.
returned :: a -> Program tx ledger signing a
returned a = Program $ \done _ -> done a

announced :: String -> a -> Program tx ledger signing a
announced message a = Program $ \done step ->
  step (Emit message) (\() -> done a)

stopped :: Program tx ledger signing a
stopped = Program $ \_ step -> step (Reject "stopped") absurd

-- Completion retains changes and effects. Waiting retains only changes.
-- Rejection discards both. Neither terminal instruction calls its continuation.
data Result a
  = Completed a [String] [String]
  | Waiting String [String]
  | Rejected String
  deriving (Eq, Show)

observe :: forall tx ledger signing a. Program tx ledger signing a -> Result a
observe (Program program) = program done step [] []
 where
  done a changes effects = Completed a (reverse changes) (reverse effects)

  step :: Instruction tx ledger signing x -> (x -> [String] -> [String] -> Result a) -> [String] -> [String] -> Result a
  step op next changes effects = case op of
    ApplyTransaction _ before tx -> next (Right (before ++ [tx])) changes effects
    SignSnapshot _ -> next (Signature "toy-signature") changes effects
    VerifySnapshot _ _ _ -> next Verified changes effects
    RecordAppliedTransaction headId _ _ _ -> next () (("applied in " ++ headId) : changes) effects
    Change event -> next () (event : changes) effects
    Emit effect -> next () changes (effect : effects)
    Decide _ answer -> next answer changes effects
    Suspend reason -> Waiting reason (reverse changes)
    Reject reason -> Rejected reason

-- After completing the instances, try :type on these examples.
transactionExample :: TransactionProgram Int (Either ValidationError [Int])
transactionExample = instruction (ApplyTransaction 0 [1, 2] 3)

snapshotExample :: SnapshotProgram Int (Signature (Snapshot Int))
snapshotExample = instruction (SignSnapshot (Snapshot [1, 2, 3]))

-- Later challenge: why does changing SnapshotProgram to TransactionProgram
-- in snapshotExample's signature cause a type error?

tests :: [(String, IO ())]
tests =
  [ ("functor/maps a returned value", assertEqual (Completed "4" [] []) $ observe (fmap show (returned (4 :: Int))))
  , ("functor/maps an instruction reply", assertEqual (Completed False [] []) $ observe (fmap not (instruction (Decide "allowed" True))))
  , ("functor/preserves instructions", assertEqual (Completed 8 [] ["first"]) $ observe (fmap (* 2) (announced "first" (4 :: Int))))
  , ("functor/does not map after rejection", assertEqual (Rejected "stopped" :: Result Int) $ observe (fmap (error "mapping function ran" :: Int -> Int) stopped))
  , ("pure/returns without instructions", assertEqual (Completed (7 :: Int) [] []) $ observe (pure 7))
  , ("pure/uses the supplied completion handler", assertEqual "answer=7" $ interpretProgram (pure (7 :: Int)) (\n -> "answer=" ++ show n) (\_ _ -> error "pure issued an instruction"))
  , ("applicative/applies a returned function", assertEqual (Completed (7 :: Int) [] []) $ observe (returned (+ 3) <*> returned 4))
  , ("applicative/preserves function-then-argument order", assertEqual (Completed (8 :: Int) [] ["function", "argument"]) $ observe (announced "function" (* 2) <*> announced "argument" 4))
  , ("applicative/uses an instruction reply", assertEqual (Completed False [] []) $ observe (returned not <*> instruction (Decide "allowed" True)))
  , ("applicative/rejection skips the argument", assertEqual (Rejected "stopped" :: Result Int) $ observe (stopped <*> error "argument program ran"))
  , ("monad/next program depends on the reply", assertEqual (Completed "yes" [] ["branch"]) $ observe (instruction (Decide "allowed" True) >>= \answer -> announced "branch" (if answer then "yes" else "no")))
  , ("monad/preserves instruction order", assertEqual (Completed (6 :: Int) [] ["first", "second"]) $ observe (announced "first" 4 >>= \n -> announced "second" (n + 2)))
  , ("monad/rejection skips the next program", assertEqual (Rejected "stopped" :: Result Int) $ observe (stopped >>= (error "next function ran" :: Int -> Program Int 'False 'False Int)))
  , ("monad/left-associated binds", assertEqual (Completed (8 :: Int) [] ["first", "second"]) $ observe ((announced "first" 3 >>= \n -> announced "second" (n + 1)) >>= \n -> returned (n * 2)))
  ,
    ( "integration/completion retains changes and effects"
    , assertEqual (Completed (42 :: Int) ["received", "applied"] ["broadcast"]) $ observe $ do
        instruction (Change "received")
        instruction (Emit "broadcast")
        instruction (Change "applied")
        pure 42
    )
  ,
    ( "integration/wait retains changes but discards effects"
    , assertEqual (Waiting "try again" ["received"] :: Result ()) $ observe $ do
        instruction (Change "received")
        instruction (Emit "broadcast")
        instruction (Suspend "try again")
        error "continued after suspension"
    )
  ,
    ( "integration/rejection discards changes and effects"
    , assertEqual (Rejected "invalid" :: Result ()) $ observe $ do
        instruction (Change "received")
        instruction (Emit "broadcast")
        instruction (Reject "invalid")
        error "continued after rejection"
    )
  ,
    ( "integration/typed ledger and signature replies"
    , assertEqual (Completed (Signature "toy-signature") [] []) $ observe $ do
        result <- instruction (ApplyTransaction 0 [1, 2 :: Int] 3)
        case result of
          Left err -> error (show err)
          Right utxo -> instruction (SignSnapshot (Snapshot utxo))
    )
  ]

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual expected actual = do
  equal <- evaluate (expected == actual)
  unless equal $ fail $ "Expected: " ++ show expected ++ "\nActual:   " ++ show actual

main :: IO ()
main = do
  filters <- getArgs
  let selected = filter (\(name, _) -> null filters || any (`isInfixOf` name) filters) tests
  if null selected
    then putStrLn "Use functor, pure, applicative, monad, integration, or no filter." >> exitFailure
    else do
      results <- forM selected $ \(name, test) -> do
        result <- try test :: IO (Either SomeException ())
        case result of
          Right () -> putStrLn ("PASS " ++ name) >> pure True
          Left err -> putStrLn ("FAIL " ++ name ++ "\n  " ++ displayException err) >> pure False
      putStrLn $ show (length (filter id results)) ++ "/" ++ show (length results) ++ " passed"
      unless (and results) exitFailure
