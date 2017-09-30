{-# LANGUAGE OverloadedStrings #-}

module Plotter where

import Data.Monoid ((<>))
import Control.Monad (forM_)
import Data.Char (ord, isDigit)
import qualified Data.ByteString.Lazy as BL (ByteString, readFile)
import qualified Data.Vector as V
import Data.List.Split (wordsBy)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime(..), midnight)
import Text.Printf (printf)

import Data.Csv
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Backend.Cairo


data StockRawData = StockRawData
  { rdate :: !String
  , ropeningPrice :: !String
  , rclosingPrice :: !String
  , rtotalVol :: !Int
  , rtrades :: !Int
  }

instance FromNamedRecord StockRawData where
  parseNamedRecord r = StockRawData <$>
    r .: "Date" <*>
    r .: "Opening price" <*>
    r .: "Closing price" <*>
    r .: "Total volume" <*>
    r .: "Trades"

data StockPoint = StockPoint
  { date :: !LocalTime
  , openingPrice :: !Double
  , closingPrice :: !Double
  , volume :: !Int
  , trades :: !Int
  }

type Error = String

parseCSVFile :: BL.ByteString -> Either Error (V.Vector StockPoint)
parseCSVFile csvRawData = do
  let options = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }
  (_, v) <- decodeByNameWith options csvRawData
  sequence $ fmap rawToStockPoint v

rawToStockPoint :: StockRawData -> Either Error StockPoint
rawToStockPoint r = StockPoint <$>
  (stringToLocalDate $ rdate r) <*>
  (stringToDouble $ ropeningPrice r) <*>
  (stringToDouble $ rclosingPrice r) <*>
  pure (rtotalVol r) <*> pure (rtrades r)

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

plotCSVData :: String -> V.Vector StockPoint -> IO ()
plotCSVData name vec = renderableToWindow (toRenderable layout) 800 800
  where price :: PlotLines LocalTime Double
        price = plot_lines_style . line_color .~ opaque blue
              $ plot_lines_values .~ [ [ (date s, closingPrice s) | s <- V.toList vec] ]
              $ plot_lines_title .~ "price"
              $ def
        layout :: LayoutLR LocalTime Double Double
        layout = layoutlr_title .~ name
               $ layoutlr_left_axis . laxis_override .~ axisGridHide
               $ layoutlr_right_axis . laxis_override .~ axisGridHide
               $ layoutlr_x_axis . laxis_override .~ axisGridHide
               $ layoutlr_plots .~ [Left (toPlot price)]
               $ layoutlr_grid_last .~ False
               $ def

main :: IO ()
main = do
  csvRawData <- BL.readFile "/home/patriques/Haskell/OMXWatcher/testres/SEB-C-2017-08-23-2017-09-22.csv"
  case parseCSVFile csvRawData of
    Left err -> putStrLn err
    Right vec -> plotCSVData "SEB-C" vec
