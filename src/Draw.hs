{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Draw (drawCanvas, canvasWidth, canvasHeight) where

import qualified GI.Gtk as Gtk
import GI.Cairo.Render hiding (x, y, width, height)
import Draw.Shape

import Data.Bits

canvasWidth :: Int
canvasWidth = 256

canvasHeight :: Int
canvasHeight = 256

data Color = RGB Double Double Double | Hex Int
color :: Color -> Render ()
color (RGB r g b) = setSourceRGB r g b
color (Hex h)     = setSourceRGB r g b
    where
        r = fromIntegral $ shiftR h 16 .&. 0xFF
        g = fromIntegral $ shiftR h 8 .&. 0xFF
        b = fromIntegral (h .&. 0xFF)


drawCanvas :: Gtk.IsWidget widget => widget -> Double -> Double -> Render ()
drawCanvas _canvas width height = do
  save

  color $ RGB 0.18 0.20 0.25
  -- setSourceRGB 0.98 0.20 0.25
  rect 0 0 width height
  fill


  restore
