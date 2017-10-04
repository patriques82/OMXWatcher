module OMXWatcher.Indicator where

import OMXWatcher.Types
import Data.List (tails)
import Data.Time.LocalTime (LocalTime(..))
import Control.Monad (forM)
import Control.Monad.State (evalState, gets, modify)

movingAverage :: Int -> Chart
movingAverage n _ | n <= 0 = error "non-positive argument"
movingAverage n xs = evalState (forM xs $ \x -> modify ((x:) . take (n-1)) >> gets average) []
  where average xs' = sum xs' / fromIntegral (length xs')
