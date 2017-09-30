module OMXWatcher.Types where

import Data.Time.LocalTime (LocalTime(..))

data StockPoint = StockPoint
  { date :: !LocalTime
  , averagePrice :: !Double
  , volume :: !Int
  , trades :: !Int
  }

type Error = String
