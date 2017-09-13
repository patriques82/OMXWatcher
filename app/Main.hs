{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Text.HTML.TagSoup
import Text.HTML.Scalpel
import Control.Monad (forM_)
import Data.Text (unpack)
import Data.Text.Encoding (decodeLatin1)
import Data.ByteString as B (readFile)

data Stock = Stock
    { sName :: String
    , sPrice :: Double
    }

main :: IO ()
main = do
    tags <- parseTags . unpack . decodeLatin1 <$> B.readFile "testres/omx.html"
    let res = scrape stockNumbers tags
    case res of
      Just nrs -> do forM_ nrs putStrLn
      Nothing -> print "nothing"

stockNumbers :: Scraper String [String]
stockNumbers = chroots selector name
  where selector = ("tr" @: [hasClass "highLight"]) // ("td" @: [hasClass "text"])

name :: Scraper String String
name = innerHTML $ "a" @: [hasClass "underline"]


price :: Scraper String Double
price = undefined


-- findNumber :: [Tag String] -> [String]
-- findNumber tags = map filterNumber $ sections (~== "<td class=text>") tags
--     where filterNumber :: [Tag String] -> String
--           filterNumber = strip . innerText . dropWhile (~/= "<td>")
--           commaToDot ',' = '.'
--           commaToDot c = c

strip :: String -> String
strip = filter (\c -> c /= '\n' || c /= '\t')
