module OMXWatcher.Types where

import           Data.Time.LocalTime (LocalTime (..))
import           Data.Vector         (Vector)

data StockPoint = StockPoint
  { date            :: !LocalTime
  , openingingPrice :: !Double
  , closingPrice    :: !Double
  , averagePrice    :: !Double
  , volume          :: !Int
  , trades          :: !Int
  }

type Error = String
