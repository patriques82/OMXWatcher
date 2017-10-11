{-# LANGUAGE OverloadedStrings #-}

module OMXWatcher.Chart (plotStockData) where

import           OMXWatcher.Indicator
import           OMXWatcher.Types

import           Data.Time.LocalTime                    (LocalTime (..))
import qualified Data.Vector                            as V (Vector, toList)
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Gtk


toLineValues :: Chart -> [StockPoint] -> [(LocalTime, Double)]
toLineValues f xs = zip dates (f xs')
  where (dates, xs') = unzip $ fmap toTimeValue xs
        toTimeValue s = (date s, closingPrice s)

plotStockData :: String -> [StockPoint] -> IO ()
plotStockData name stocks = renderableToWindow (toRenderable layout) 800 800
  where
--        price :: PlotLines LocalTime Double
--        price = plot_lines_style . line_color .~ opaque blue
--              $ plot_lines_values .~ [ [ (date s, averagePrice s) | s <- stocks] ]
--              $ plot_lines_title .~ "price"
--              $ def
        macd :: PlotLines LocalTime Double
        macd  = plot_lines_style . line_color .~ opaque black
              $ plot_lines_values .~ [ toLineValues (macdLine 12 26) stocks ]
              $ plot_lines_title .~ "macd"
              $ def
        signal :: PlotLines LocalTime Double
        signal = plot_lines_style . line_color .~ opaque red
              $ plot_lines_values .~ [ toLineValues (signalLine 9 12 26) stocks ]
              $ plot_lines_title .~ "signal"
              $ def
--        mAvg :: PlotLines LocalTime Double
--        mAvg = plot_lines_style . line_color .~ opaque red
--              $ plot_lines_values .~ [ toLineValues (movingAverage 10) stocks ]
--              $ plot_lines_title .~ "mAvg"
--              $ def
        layout :: LayoutLR LocalTime Double Double
        layout = layoutlr_title .~ name
               $ layoutlr_left_axis . laxis_override .~ axisGridHide
               $ layoutlr_right_axis . laxis_override .~ axisGridHide
               $ layoutlr_x_axis . laxis_override .~ axisGridHide
               -- $ layoutlr_plots .~ [Left (toPlot price), Left (toPlot mAvg)]
               $ layoutlr_plots .~ [Left (toPlot macd), Left (toPlot signal)]
               $ layoutlr_grid_last .~ False
               $ def
