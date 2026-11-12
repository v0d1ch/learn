{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Werror #-}

-- | A standalone exercise; only base is needed.
--
-- Run from the learn_haskell directory:
--   cabal run exe:free-monad
--   cabal run exe:free-monad -- tree
--   cabal run exe:free-monad -- church
--   cabal run exe:free-monad -- 'tree/fmap'
--
-- Open GHCi with: cabal repl exe:free-monad
-- Then: :main tree
-- After editing: :reload
--
-- Replace each TODO. Suggested order:
--   tree fmap, bind, Applicative, instruction, foldProgram;
--   church pure, instructionC, fmap, bind, Applicative.
-- Tests below the exercise are supplied; leave them unchanged.
module Main where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM, unless)
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.Exit (exitFailure)

data Instruction a where
  ReadNumber :: Instruction Int
  PrintNumber :: Int -> Instruction ()

data Program a where
  Pure :: a -> Program a
  Step :: Instruction x -> (x -> Program a) -> Program a

instance Functor Program where
  fmap f (Pure a) = Pure (f a)
  fmap f (Step i c) = Step i (\x -> fmap f (c x))

bind :: Program a -> (a -> Program b) -> Program b
bind (Pure a) next = next a
bind (Step instruction continue) next = Step instruction (\x -> bind (continue x) next)

instance Applicative Program where
  pure = Pure
  (<*>) (Step instruction continue) as =
    Step instruction (\x -> continue x <*> as)
  (<*>) (Pure f) as = fmap f as

instance Monad Program where
  (>>=) = bind

instruction :: Instruction a -> Program a
instruction i = Step i (\x -> Pure x)

-- Interpret one instruction, feed its result to the continuation, and continue.
foldProgram :: Monad m => (forall x. Instruction x -> m x) -> Program a -> m a
foldProgram _ (Pure a) = pure a
foldProgram f (Step instruction next) = f instruction >>= (\a -> foldProgram f (next a))

-- Optional interactive demo: runProgram example
runProgram :: Program a -> IO a
runProgram = foldProgram interpret
 where
  interpret :: Instruction x -> IO x
  interpret ReadNumber = readLn
  interpret (PrintNumber n) = print n

example :: Program ()
example = do
  n <- instruction ReadNumber
  instruction (PrintNumber (n + 1))

newtype ProgramC a = ProgramC
  { runProgramC ::
      forall r.
      (a -> r) ->
      (forall x. Instruction x -> (x -> r) -> r) ->
      r
  }

instance Functor ProgramC where
  fmap f (ProgramC program) = ProgramC $ \done step -> program (\a -> done (f a)) step

instance Applicative ProgramC where
  pure a = ProgramC $ \done step -> done a
  (<*>) (ProgramC functions) (ProgramC values) = ProgramC $ \done step -> functions (\f -> values (\a -> done (f a)) step) step

instance Monad ProgramC where
  (>>=) (ProgramC program) f = ProgramC $ \done step -> program (\a -> let ProgramC x = (f a) in x done step) step

instructionC :: Instruction a -> ProgramC a
instructionC i = ProgramC $ \done step -> step i done

exampleC :: ProgramC ()
exampleC = do
  n <- instructionC ReadNumber
  instructionC (PrintNumber (n + 1))

-- Supplied test support. Observations include the result, printed numbers,
-- and unused input, so tests check ordering as well as returned values.
observe :: Program a -> [Int] -> Either String (a, [Int], [Int])
observe (Pure a) inputs = Right (a, [], inputs)
observe (Step ReadNumber next) inputs = case inputs of
  [] -> Left "No input left"
  n : rest -> observe (next n) rest
observe (Step (PrintNumber n) next) inputs = do
  (a, printed, rest) <- observe (next ()) inputs
  pure (a, n : printed, rest)

observeC :: ProgramC a -> [Int] -> Either String (a, [Int], [Int])
observeC program = observe (runProgramC program Pure Step)

-- Direct constructors keep the early tests independent of instruction/bind.
readTree :: Program Int
readTree = Step ReadNumber Pure

printTree :: Int -> Program ()
printTree n = Step (PrintNumber n) Pure

readChurch :: ProgramC Int
readChurch = ProgramC $ \done step -> step ReadNumber done

printChurch :: Int -> ProgramC ()
printChurch n = ProgramC $ \done step -> step (PrintNumber n) done

pureChurch :: a -> ProgramC a
pureChurch a = ProgramC $ \done _ -> done a

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual expected actual = do
  equal <- evaluate (expected == actual)
  unless equal $ fail $ "Expected: " <> show expected <> "\nActual:   " <> show actual

-- A deterministic interpreter for foldProgram; no terminal input is required.
fixedInput :: Instruction a -> Maybe a
fixedInput ReadNumber = Just 7
fixedInput (PrintNumber _) = Just ()

rejectPrint :: Instruction a -> Maybe a
rejectPrint ReadNumber = Just 7
rejectPrint (PrintNumber _) = Nothing

tests :: [(String, IO ())]
tests =
  [ ("tree/fmap pure", assertEqual (Right (4, [], [])) $ observe (fmap (+ 1) (Pure (3 :: Int))) [])
  , ("tree/fmap continuation", assertEqual (Right (8, [4], [])) $ observe (fmap (* 2) (Step ReadNumber (\n -> Step (PrintNumber n) (\() -> Pure n)))) [4])
  , ("tree/bind pure", assertEqual (Right ((), [3], [])) $ observe (bind (Pure 3) printTree) [])
  , ("tree/bind dependent steps", assertEqual (Right ((), [8, 9], [99])) $ observe (bind readTree (\n -> Step (PrintNumber n) (\() -> printTree (n + 1)))) [8, 99])
  , ("tree/applicative order", assertEqual (Right (7, [], [99])) $ observe ((-) <$> readTree <*> readTree) [10, 3, 99])
  , ("tree/instruction read", assertEqual (Right (5, [], [6])) $ observe (instruction ReadNumber) [5, 6])
  , ("tree/instruction print", assertEqual (Right ((), [5], [6])) $ observe (instruction (PrintNumber 5)) [6])
  , ("tree/fold pure", assertEqual (Just (3 :: Int)) $ foldProgram fixedInput (Pure 3))
  , ("tree/fold continuation", assertEqual (Just 8) $ foldProgram fixedInput (Step ReadNumber (\n -> Pure (n + 1))))
  , ("tree/fold interpreter failure", assertEqual (Nothing :: Maybe ()) $ foldProgram rejectPrint (printTree 3))
  , ("tree/example", assertEqual (Right ((), [42], [99])) $ observe example [41, 99])
  , ("church/pure", assertEqual (Right (3, [], [99])) $ observeC (pure (3 :: Int)) [99])
  , ("church/instruction read", assertEqual (Right (5, [], [6])) $ observeC (instructionC ReadNumber) [5, 6])
  , ("church/instruction print", assertEqual (Right ((), [5], [6])) $ observeC (instructionC (PrintNumber 5)) [6])
  , ("church/fmap pure", assertEqual (Right (4, [], [])) $ observeC (fmap (+ 1) (pureChurch (3 :: Int))) [])
  , ("church/fmap continuation", assertEqual (Right (8, [], [])) $ observeC (fmap (* 2) readChurch) [4])
  , ("church/bind pure", assertEqual (Right ((), [3], [])) $ observeC (pureChurch 3 >>= printChurch) [])
  , ("church/bind dependent steps", assertEqual (Right ((), [8, 9], [99])) $ observeC (readChurch >>= \n -> printChurch n >> printChurch (n + 1)) [8, 99])
  , ("church/applicative order", assertEqual (Right (7, [], [99])) $ observeC ((-) <$> readChurch <*> readChurch) [10, 3, 99])
  , ("church/example", assertEqual (Right ((), [42], [99])) $ observeC exampleC [41, 99])
  , ("church/left-associated binds", assertEqual (Right (8, [], [])) $ observeC ((readChurch >>= \n -> pureChurch (n + 1)) >>= \n -> pureChurch (n * 2)) [3])
  ]

main :: IO ()
main = do
  filters <- getArgs
  let selected = filter (\(name, _) -> null filters || any (`isInfixOf` name) filters) tests
  if null selected
    then putStrLn "No matching tests. Try tree, church, or tree/fmap." >> exitFailure
    else do
      results <- forM selected $ \(name, test) -> do
        result <- try test :: IO (Either SomeException ())
        case result of
          Right () -> putStrLn ("PASS " <> name) >> pure True
          Left err -> putStrLn ("FAIL " <> name <> "\n  " <> displayException err) >> pure False
      putStrLn $ show (length $ filter id results) <> "/" <> show (length results) <> " passed"
      unless (and results) exitFailure
