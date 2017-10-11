module OMXWatcher.IndicatorTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           OMXWatcher.Indicator

unitTests =
  let macdLineTest = map round (macdLine 6 10 testData)
      signalLineTest = map round (signalLine 3 6 9 testData)
      movingAverageTest = map round (movingAverage 3 testData)
  in testGroup "Unit tests"
    [ testCase "macdLine functional" $
        macdLineTest @?= [-7,-7,-5,-4,-3,-3,-3]
    , testCase "signalLine functional" $
        signalLineTest @?= [-4,-4,-3,-2,-2]
    , testCase "movingAverage functional" $
        movingAverageTest @?= [460,454,452,449,447,448,445,445,439,431,427,426,429,429,433,433,432]
    , testCase "intersectionEvent buy signal" $
        intersectionEvent (3.1, 2.513) (2.4, 2.514) @?= Just Buy
    , testCase "intersectionEvent sell signal" $
        intersectionEvent (2.4, 2.514) (3.5, 2.513) @?= Just Sell
    , testCase "intersectionEvent nothing" $
        intersectionEvent (2.4, 2.522) (3.5, 2.522) @?= Nothing
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
