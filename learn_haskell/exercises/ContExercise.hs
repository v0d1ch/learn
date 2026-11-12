{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Continuation exercises using only base.
--
-- From the learn_haskell directory:
--   cabal repl exe:cont
-- In GHCi:
--   :main functor
--   :main pure
--   :main applicative
--   :main monad
--   :main
-- Save edits, then use :reload before running tests again.
--
-- Replace the four TODOs. Implement Applicative without using bind first.
-- Use a typed hole (_) inside your attempt to inspect the available types.
-- For more relevant bindings: :set -fno-max-relevant-binds
--
-- Unlike ProgramC, Cont fixes r in its type and has no instruction handler.
-- a is the value passed to the continuation; r is the overall result.
module Main where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM, unless)
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.Exit (exitFailure)

newtype Cont r a = Cont
  { runCont :: (a -> r) -> r
  }

instance Functor (Cont r) where
  fmap f (Cont c) = Cont $ \done -> c $ \a -> done (f a)

instance Applicative (Cont r) where
  pure a = Cont $ \done -> done a
  (<*>) (Cont f) (Cont a) = Cont $ \done -> f $ (\y -> a (\x -> done $ y x))

instance Monad (Cont r) where
  (>>=) (Cont a) f = Cont $ \done -> a $ \x -> let Cont t = f x in t done

-- Supplied tests: leave this section unchanged.
-- Some computations invoke their continuation twice or ignore it entirely.
-- These cases distinguish Cont from a simple container holding one value.
assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual expected actual = do
  equal <- evaluate (expected == actual)
  unless equal $ fail $ "Expected: " <> show expected <> "\nActual:   " <> show actual

tests :: [(String, IO ())]
tests =
  [
    ( "functor/transforms the supplied value"
    , assertEqual "number=5" $
        runCont (fmap (+ 1) (Cont (\done -> done (4 :: Int)))) (\n -> "number=" <> show n)
    )
  ,
    ( "functor/preserves work around the continuation"
    , assertEqual "before[5]after" $
        runCont (fmap (+ 1) (Cont (\done -> "before[" <> done (4 :: Int) <> "]after"))) show
    )
  ,
    ( "functor/maps both delivered values"
    , assertEqual [11, 12 :: Int] $
        runCont (fmap (+ 10) (Cont (\done -> done 1 <> done 2))) (: [])
    )
  ,
    ( "functor/leaves an ignored continuation unused"
    , assertEqual "stopped" $
        runCont (fmap (error "mapping function must not run" :: Int -> Int) (Cont (\_ -> "stopped"))) show
    )
  ,
    ( "pure/passes its value to the continuation"
    , assertEqual (21 :: Int) $ runCont (pure 7) (* 3)
    )
  ,
    ( "pure/value and result can have different types"
    , assertEqual "True" $ runCont (pure True) show
    )
  ,
    ( "applicative/applies the produced function"
    , assertEqual "7" $
        runCont (Cont (\done -> done (+ 3)) <*> Cont (\done -> done (4 :: Int))) show
    )
  ,
    ( "applicative/runs function before argument"
    , assertEqual "function>argument>result=8" $
        runCont
          (Cont (\done -> "function>" <> done (* 2)) <*> Cont (\done -> "argument>" <> done (4 :: Int)))
          (\n -> "result=" <> show n)
    )
  ,
    ( "applicative/combines both function and value choices in order"
    , assertEqual [11, 12, 10, 20 :: Int] $
        runCont
          (Cont (\done -> done (+ 10) <> done (* 10)) <*> Cont (\done -> done 1 <> done 2))
          (: [])
    )
  ,
    ( "applicative/can skip the argument computation"
    , assertEqual "stopped" $
        runCont
          ((Cont (\_ -> "stopped") :: Cont String (Int -> Int)) <*> error "argument must not run")
          show
    )
  ,
    ( "applicative/final continuation can change the result type"
    , assertEqual True $
        runCont (Cont (\done -> done (+ 3)) <*> Cont (\done -> done (4 :: Int))) (== 7)
    )
  ,
    ( "monad/next computation depends on the previous value"
    , assertEqual "large" $
        runCont
          ( Cont (\done -> done (4 :: Int)) >>= \n ->
              if n > 3 then Cont (\done -> done "large") else Cont (\done -> done "small")
          )
          id
    )
  ,
    ( "monad/preserves continuation nesting"
    , assertEqual "first[second[6]second]first" $
        runCont
          ( Cont (\done -> "first[" <> done (4 :: Int) <> "]first") >>= \n ->
              Cont (\done -> "second[" <> done (n + 2) <> "]second")
          )
          show
    )
  ,
    ( "monad/handles each invocation of the first continuation"
    , assertEqual [10, 20 :: Int] $
        runCont
          (Cont (\done -> done 1 <> done 2) >>= \n -> Cont (\done -> done (n * 10)))
          (: [])
    )
  ,
    ( "monad/can skip the next computation"
    , assertEqual "stopped" $
        runCont
          ((Cont (\_ -> "stopped") :: Cont String Int) >>= (error "next computation must not run" :: Int -> Cont String Int))
          show
    )
  ,
    ( "monad/do notation integration"
    , assertEqual "answer=42" $
        runCont
          ( do
              n <- pure (20 :: Int)
              doubled <- pure (* 2) <*> pure n
              fmap (+ 2) (pure doubled)
          )
          (\n -> "answer=" <> show n)
    )
  ]

main :: IO ()
main = do
  filters <- getArgs
  let selected = filter (\(name, _) -> null filters || any (`isInfixOf` name) filters) tests
  if null selected
    then putStrLn "No matching tests. Try functor, pure, applicative, or monad." >> exitFailure
    else do
      results <- forM selected $ \(name, test) -> do
        result <- try test :: IO (Either SomeException ())
        case result of
          Right () -> putStrLn ("PASS " <> name) >> pure True
          Left err -> putStrLn ("FAIL " <> name <> "\n  " <> displayException err) >> pure False
      putStrLn $ show (length $ filter id results) <> "/" <> show (length results) <> " passed"
      unless (and results) exitFailure
