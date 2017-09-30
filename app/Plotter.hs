{-# LANGUAGE OverloadedStrings #-}

module Plotter where

import OMXWatcher.Csv (parseCSV)
import OMXWatcher.Chart (plotCSVData)
import qualified Data.ByteString.Lazy as BL (readFile)

main :: IO ()
main = do
  csvRaw <- BL.readFile "/home/patriques/Haskell/OMXWatcher/testres/SEB-C-2017-08-23-2017-09-22.csv"
  case parseCSV csvRaw of
    Left err -> putStrLn err
    Right vec -> plotCSVData "SEB-C" vec
