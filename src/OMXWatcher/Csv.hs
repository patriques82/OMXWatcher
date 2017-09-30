{-# LANGUAGE OverloadedStrings #-}

module OMXWatcher.Csv (parseCSV) where

import OMXWatcher.Types

import Data.Char (ord, isDigit)
import Data.ByteString.Lazy (ByteString)
import Data.Vector (Vector)
import Data.List.Split (wordsBy)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime(..), midnight)
import Data.Csv

data StockRawData = StockRawData
  { rdate :: !String
  , ropeningPrice :: !String
  , rclosingPrice :: !String
  , raveragePrice :: !String
  , rtotalVol :: !Int
  , rtrades :: !Int
  }

instance FromNamedRecord StockRawData where
  parseNamedRecord r = StockRawData <$>
    r .: "Date" <*>
    r .: "Opening price" <*>
    r .: "Closing price" <*>
    r .: "Average price" <*>
    r .: "Total volume" <*>
    r .: "Trades"

parseCSV :: ByteString -> Either Error (Vector StockPoint)
parseCSV csvRawData = do
  let options = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }
  (_, v) <- decodeByNameWith options csvRawData
  sequence $ fmap rawToStockPoint v

rawToStockPoint :: StockRawData -> Either Error StockPoint
rawToStockPoint r = StockPoint <$>
  (stringToLocalDate $ rdate r) <*>
  (stringToDouble $ raveragePrice r) <*>
  pure (rtotalVol r) <*>
  pure (rtrades r)

stringToLocalDate :: String -> Either Error LocalTime
stringToLocalDate s = do
  (yyyy, mm, dd) <- getYearMonthDay
  return $ LocalTime (fromGregorian (read yyyy) (read mm) (read dd)) midnight
    where getYearMonthDay =
            case splitNumbers s of
              [yyyy, mm, dd] -> Right (yyyy, mm, dd)
              x -> Left $ "Not valid date: " ++ (show x)

stringToDouble :: String -> Either Error Double
stringToDouble s = do
  (n, d) <- getNumDecimal
  return $ read (n ++ '.':d)
    where getNumDecimal =
            case splitNumbers s of
              [n, d] -> Right (n, d)
              x -> Left $ "Not valid double: " ++ (show x)

splitNumbers = wordsBy (not . isDigit)