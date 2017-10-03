module OMXWatcher.Indicator where

import OMXWatcher.Types
import Data.Time.LocalTime (LocalTime(..))
import Control.Monad (forM)
import Control.Monad.State (evalState, gets, modify)

toTimeValues :: [StockPoint] -> [(LocalTime, Double)]
toTimeValues = fmap (\x -> (date x, closingPrice x))

movingAverage :: Int -> [(LocalTime, Double)] -> [(LocalTime, Double)]
movingAverage n _  | n <= 0 = error "non-positive argument"
movingAverage n timeValues = zip dates newValues
  where average xs = sum xs / fromIntegral (length xs)
        (dates, oldValues) = unzip timeValues
        newValues = evalState (forM oldValues $ \x -> modify ((x:) . take (n-1)) >> gets average) []
