module OMXWatcher.Indicator (movingAverage) where

import OMXWatcher.Types
import Data.List (tails)
import Data.Time.LocalTime (LocalTime(..))
import Control.Monad (forM)
import Control.Monad.State (evalState, gets, modify)

movingAverage :: (Fractional a) => Int -> [a] -> [a]
movingAverage n _ | n <= 0 = error "non-positive argument"
movingAverage n xs = fmap average $ groupBy n xs
  where average xs' = sum xs' / fromIntegral (length xs')

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy n xs = go [] xs
  where
    go _ []      = []
    go l (x:xs') = (x:t) : go (x:l) xs'
      where t = take (n-1) l
