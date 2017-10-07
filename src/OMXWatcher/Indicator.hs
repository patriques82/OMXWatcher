module OMXWatcher.Indicator where

import Debug.Trace

-- MACD

signalLine :: (Fractional a) => [a] -> [a]
signalLine = undefined

macd :: (Fractional a) => [a] -> [a]
macd = undefined

ema :: (Fractional a) => Int -> [a] -> [a]
ema n xs
  | length xs < n = error "list length < n"
  | otherwise = go (average h) t
     where
       (h, t) = splitAt n xs
       go _ [] = []
       go prev (x:xs') =
         let curr = x * (2 / fromIntegral (n+1)) + prev * (1 - (2 / fromIntegral (n+1)))
         in curr : go curr xs'

-- Simple moving average

movingAverage :: (Fractional a) => Int -> [a] -> [a]
movingAverage n _ | n <= 0 = error "non-positive argument"
movingAverage n xs = fmap average $ movingWindow n xs

-- Utility

average :: (Fractional a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)

movingWindow :: Int -> [a] -> [[a]]
movingWindow _ [] = []
movingWindow n xs = go [] xs
  where
    go _ []      = []
    go l (x:xs') = (x:t) : go (x:l) xs'
      where t = take (n-1) l
