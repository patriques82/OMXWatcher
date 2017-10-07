module Main where

import Test.Tasty

import qualified OMXWatcher.IndicatorTest


main :: IO ()
main = defaultMain tests

tests = testGroup "Tests"
  [ OMXWatcher.IndicatorTest.unitTests
  ]
