{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Draw (drawCanvas, canvasWidth, canvasHeight) where

import qualified GI.Gtk as Gtk
import GI.Cairo.Render hiding (x, y, width, height)
import Draw.Shape

canvasWidth :: Int
canvasWidth = 256

canvasHeight :: Int
canvasHeight = 256

drawCanvas :: Gtk.IsWidget widget => widget -> Double -> Double -> Render ()
drawCanvas _canvas width height = do
  save

  setSourceRGB 0.18 0.20 0.25
  rect 0 0 width height
  fill

  restore
