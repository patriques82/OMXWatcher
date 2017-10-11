{-# LANGUAGE OverloadedStrings #-}

module Plotter where

import OMXWatcher.Csv (parseCSV)
import OMXWatcher.Chart (plotStockData)

--import Control.Monad (guard)
--import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL (readFile)

main :: IO ()
main = do
--  args <- getArgs -- pathToCSVfile
--  guard (not . null $ args)
  csvRaw <- BL.readFile "/home/patriques/Haskell/OMXWatcher/testres/SEB-C-2016-10-10-2017-10-06.csv"
  case parseCSV csvRaw of
    Left err -> putStrLn err
    Right stocks -> plotStockData "SEB-C" stocks
