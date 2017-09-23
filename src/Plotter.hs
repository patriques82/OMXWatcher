{-# LANGUAGE OverloadedStrings #-}

module Plotter where

import Data.Monoid ((<>))
import Data.Char (ord)
import qualified Data.ByteString.Lazy as BL (ByteString, readFile)
import qualified Data.Vector as V

import Data.Csv

-- Date; Bid; Ask; Opening price; High price; Low price; Closing price; Average price; Total volume; Turnover; Trades;
-- 2017-09-22;103,60;103,90;104,00;104,00;102,90;104,00;103,591;20877;2162673,1;156;
data StockRawData = StockRawData
    { date :: !String
    , bid :: !String
    , ask :: !String
    , openingPrice :: !String
    , highPrice :: !String
    , lowPrice :: !String
    , closingPrice :: !String
    , avgPrice :: !String
    , totalVol :: !String
    , turnOver :: !String
    , trades :: !String
    }

instance Show StockRawData where
  show s = date s <> " -- " <> closingPrice s

instance FromNamedRecord StockRawData where
    parseNamedRecord r = StockRawData <$>
      r .: "Date" <*>
      r .: "Bid" <*>
      r .: "Ask" <*>
      r .: "Opening price" <*>
      r .: "High price" <*>
      r .: "Low price" <*>
      r .: "Closing price" <*>
      r .: "Average price" <*>
      r .: "Total volume" <*>
      r .: "Turnover" <*>
      r .: "Trades"

parseCSVFile :: BL.ByteString -> Either String (V.Vector StockRawData)
parseCSVFile csvRawData =
  let options = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }
  in snd <$> decodeByNameWith options csvRawData

main :: IO ()
main = do
  csvRawData <- BL.readFile "../testres/SEB-C-2017-08-23-2017-09-22.csv"
  case parseCSVFile csvRawData of
    Left err -> putStrLn err
    Right vec -> V.forM_ vec (putStrLn . show)
