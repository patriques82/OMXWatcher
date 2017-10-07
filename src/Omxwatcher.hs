{-# LANGUAGE OverloadedStrings #-}

module Omxwatcher where

import Data.Text (unpack)
import Data.Text.Encoding (decodeLatin1)
import Data.List.Split (chunksOf)
import Data.ByteString as B (readFile)
import Control.Monad (forM_)
import Text.HTML.Scalpel
import Text.HTML.TagSoup (parseTags, Tag)

data Stock = Stock
    { name :: String
    , latestPrice :: String
    , priceChange :: String
    , priceChangePercent :: String
    , highest :: String
    , lowest :: String
    , volume :: String
    } deriving Show

main :: IO ()
main = do
  file <- B.readFile "testres/omx.html"
  let stocks = scrapeStocks . parseTags . unpack . decodeLatin1 $ file
  forM_ stocks print

scrapeStocks :: [Tag String] -> Maybe [Stock]
scrapeStocks tags = mkStocks <$> scrape stockData tags

stockData :: Scraper String [String]
stockData = chroots (("tr" @: [hasClass "highLight"]) // "td") (text "td")

mkStocks :: [String] -> [Stock]
mkStocks rawdata = mkStock <$> chunksOf 12 rawdata
  where mkStock (_:xs) = parseStockData xs

parseStockData :: [String] -> Stock
parseStockData [n, lp, pc, pcp, _, _, h, l, v, _, _] =
  Stock n lp pc pcp h l v
