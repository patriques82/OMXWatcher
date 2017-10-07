{-# LANGUAGE OverloadedStrings #-}

module OMXWatcher.Chart (plotStockData) where

import OMXWatcher.Types
import OMXWatcher.Indicator

import qualified Data.Vector as V (Vector, toList)
import Data.Time.LocalTime (LocalTime(..))
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Backend.Cairo


renderChart :: ([Double] -> [Double]) -> [StockPoint] -> [(LocalTime, Double)]
renderChart f xs = zip dates (f vals)
  where (dates, vals) = unzip $ fmap toTimeValue xs
        toTimeValue s = (date s, closingPrice s)

plotStockData :: String -> [StockPoint] -> IO ()
plotStockData name stocks = renderableToWindow (toRenderable layout) 800 800
  where price :: PlotLines LocalTime Double
        price = plot_lines_style . line_color .~ opaque blue
              $ plot_lines_values .~ [ [ (date s, averagePrice s) | s <- stocks] ]
              $ plot_lines_title .~ "price"
              $ def
        mAvg :: PlotLines LocalTime Double
        mAvg  = plot_lines_style . line_color .~ opaque red
              $ plot_lines_values .~ [ renderChart (movingAverage 10) stocks ]
              $ plot_lines_title .~ "moving average"
              $ def
        layout :: LayoutLR LocalTime Double Double
        layout = layoutlr_title .~ name
               $ layoutlr_left_axis . laxis_override .~ axisGridHide
               $ layoutlr_right_axis . laxis_override .~ axisGridHide
               $ layoutlr_x_axis . laxis_override .~ axisGridHide
               $ layoutlr_plots .~ [Left (toPlot price), Left (toPlot mAvg)]
               $ layoutlr_grid_last .~ False
               $ def
