module OMXWatcher.IndicatorTest where

import Test.Tasty
import Test.Tasty.HUnit

import OMXWatcher.Indicator
import Data.List
import Data.Ord

unitTests = testGroup "Unit tests"
  [ testCase "ema" $
      (map round $ ema 12 testData) @?= [439, 438, 438, 437, 435]
  ]

testData =
  [ 459.99
  , 448.85
  , 446.06
  , 450.81
  , 442.80
  , 448.97
  , 444.57
  , 441.40
  , 430.47
  , 420.05
  , 431.14
  , 425.66
  , 430.58
  , 431.72
  , 437.87
  , 428.43
  , 428.35
  ]
