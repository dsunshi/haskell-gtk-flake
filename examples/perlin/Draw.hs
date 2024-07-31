{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Draw (drawCanvas, canvasWidth, canvasHeight) where

import qualified GI.Gtk as Gtk
import GI.Cairo.Render hiding (x, y, width, height)
import Draw.Shape
import Draw.Color
import Draw.Noise

canvasWidth :: Int
canvasWidth = 500

canvasHeight :: Int
canvasHeight = 500

perlin :: Double -> Double -> Render ()
perlin x y = do
    color $ Gray ((1.0 + noise x y 0) / 2.0) -- [-1 .. 1] -> [0 .. 1]
    point x y

canvasPoints :: [(Double, Double)]
canvasPoints = [(x, y)
  | x <- [1.0 .. fromIntegral canvasWidth]
  , y <- [1.0 .. fromIntegral canvasHeight] ]

drawCanvas :: Gtk.IsWidget widget => widget -> Double -> Double -> Render ()
drawCanvas _canvas width height = do
  save

  -- Draw a background. Try removing this to see what happens!
  color $ Hex 0x000000
  rect 0 0 width height

  mapM_ (uncurry perlin) canvasPoints

  restore
