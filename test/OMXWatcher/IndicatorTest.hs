module OMXWatcher.IndicatorTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           OMXWatcher.Indicator

unitTests =
  let emaTest = map round (ema 12 testData)
      macdLineTest = map round (macdLine 6 10 testData)
      signalLineTest = map round (signalLine 3 6 9 testData)
      movingAverageTest = map round (movingAverage 3 testData)
  in testGroup "Unit tests"
    [ testCase "ema functional" $
        emaTest @?= [439,438,438,437,435]
    , testCase "size of ema short - size of ema long = long - short" $
        length (ema 6 testData) - length (ema 10 testData) @?= 4
    , testCase "macdLine functional" $
        macdLineTest @?= [-7,-7,-5,-4,-3,-3,-3]
    , testCase "signalLine functional" $
        signalLineTest @?= [-4,-4,-3,-2,-2]
    , testCase "movingAverage functional" $
        movingAverageTest @?= [460,454,452,449,447,448,445,445,439,431,427,426,429,429,433,433,432]
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
