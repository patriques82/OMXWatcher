module OMXWatcher.Types where

import Data.Vector (Vector)
import Data.Time.LocalTime (LocalTime(..))

data StockPoint = StockPoint
  { date :: !LocalTime
  , openingingPrice :: !Double
  , closingPrice :: !Double
  , averagePrice :: !Double
  , volume :: !Int
  , trades :: !Int
  }

type TimeValue = (LocalTime, Double)

type Error = String
