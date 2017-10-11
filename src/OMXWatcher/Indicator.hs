module OMXWatcher.Indicator
  ( Chart
  , Event(..)
  , Plan
  , signalLine
  , macdLine
  , movingAverage
  , intersectionEvent
  , macdStrategy
  ) where

type Chart = [Double] -> [Double]

data Event = Buy | Sell
  deriving (Eq, Show)

type Plan = [Maybe Event]

type Strategy = [Double] -> Plan

type Segment = (Double, Double)

macdStrategy :: Int -> Int -> Int -> Strategy
macdStrategy m s l xs = zipWith intersectionEvent sL mL
  where
    sL = makeSegments $ signalLine m s l xs
    mL = makeSegments $ macdLine s l xs

intersectionEvent :: Segment -> Segment -> Maybe Event
intersectionEvent (a1, a2) (b1, b2)
  | a1 > b1 && a2 < b2 = Just Buy
  | a1 < b1 && a2 > b2 = Just Sell
  | otherwise = Nothing


makeSegments :: [Double] -> [Segment]
makeSegments xs = zip (drop 1 xs) xs

--------------------------------------------------------------------

signalLine :: Int -> Int -> Int -> Chart
signalLine macd short long = ema macd . macdLine short long

macdLine :: Int -> Int -> Chart
macdLine short long xs
  | short >= long = error "first argument >= second argument"
  | otherwise =
    let emaShort = ema short xs
        emaLong = ema long xs
    in zipWith (-) (drop (long - short) emaShort) emaLong

-- Utility
ema :: Int -> Chart
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

--------------------------------------------------------------------

movingAverage :: Int -> Chart
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
