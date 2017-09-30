{-# LANGUAGE OverloadedStrings #-}

module OMXWatcher.Chart (plotCSVData) where

import OMXWatcher.Types

import qualified Data.Vector as V (Vector, toList)
import Data.Time.LocalTime (LocalTime(..))

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Backend.Cairo

plotCSVData :: String -> V.Vector StockPoint -> IO ()
plotCSVData name vec = renderableToWindow (toRenderable layout) 800 800
  where price :: PlotLines LocalTime Double
        price = plot_lines_style . line_color .~ opaque blue
              $ plot_lines_values .~ [ [ (date s, averagePrice s) | s <- V.toList vec] ]
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
