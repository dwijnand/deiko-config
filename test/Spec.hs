{-# LANGUAGE RecordWildCards #-}

module Spec(main) where

import System.Exit
import Test.HUnit

import qualified AesonSpec

main :: IO ()
main = do
  Counts{..} <- runTestTT tests
  if errors + failures == 0 && tried > 0
      then exitSuccess
      else exitFailure

tests = TestList
    [ AesonSpec.tests
    ]
