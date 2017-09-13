{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Text (unpack)
import Data.Text.Encoding (decodeLatin1)
import Data.List.Split (chunksOf)
import Data.ByteString as B (readFile)
import Control.Monad (forM_, guard)
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
  tags <- parseTags . unpack . decodeLatin1 <$> B.readFile "testres/omx.html"
  let stocks = scrapeStocks tags
  forM_ stocks (putStrLn . show)

scrapeStocks :: [Tag String] -> Maybe [Stock]
scrapeStocks tags = mkStocks <$> scrape stockData tags

stockData :: Scraper String [(Int, String)]
stockData = chroots (("tr" @: [hasClass "highLight"]) // "td") $ do
  index <- position
  content <- text "td"
  return (index, content)

mkStocks :: [(Int, String)] -> [Stock]
mkStocks rawdata = mkStock <$> groups
  where groups = chunksOf 12 rawdata
        mkStock (_:xs) = parseStockData $ snd (unzip xs)

parseStockData :: [String] -> Stock
parseStockData [n, p, c, cp, _, _, h, l, v, _, _] =
  Stock n p c cp h l v
