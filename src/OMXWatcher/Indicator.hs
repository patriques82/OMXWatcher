module OMXWatcher.Indicator
  ( signalLine
  , macdLine
  , ema
  , movingAverage
  ) where

-- MACD

signalLine :: (Fractional a) => Int -> Int -> Int -> [a] -> [a]
signalLine macd short long = ema macd . macdLine short long

-- macdLine 12 26 xs
macdLine :: (Fractional a) => Int -> Int -> [a] -> [a]
macdLine short long xs
  | short >= long = error "first argument >= second argument"
  | otherwise =
    let emaShort = ema short xs
        emaLong = ema long xs
    in zipWith (-) (drop (long - short) emaShort) emaLong

ema :: (Fractional a) => Int -> [a] -> [a]
ema n xs
  | length xs < n = error "list length < n"
  | otherwise = go (average h) t
  where
    (h, t) = splitAt n xs
    go _ [] = []
    go prev (x:xs') =
      let denom = fromIntegral (n + 1)
          curr = x * (2 / denom) + prev * (1 - (2 / denom))
      in curr : go curr xs'

-- Simple moving average
movingAverage :: (Fractional a) => Int -> [a] -> [a]
movingAverage n _
  | n <= 0 = error "non-positive argument"
movingAverage n xs = average <$> movingWindow n xs

-- Utility
average :: (Fractional a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)

movingWindow :: Int -> [a] -> [[a]]
movingWindow _ [] = []
movingWindow n xs = go [] xs
  where
    go _ [] = []
    go l (x:xs') = (x : t) : go (x : l) xs'
      where
        t = take (n - 1) l
