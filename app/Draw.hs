{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Draw (drawCanvas, canvasWidth, canvasHeight) where

import qualified GI.Gtk as Gtk
import GI.Cairo.Render hiding (x, y, width, height)
import Draw.Shape
import Draw.Color
import Draw.Path
import Draw.Noise

canvasWidth :: Int
canvasWidth = 400

canvasHeight :: Int
canvasHeight = 400

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

  color $ Hex 0x4c566a
  rect 0 0 width height

  color $ Hex 0x2e3440
  rect 10 10 (width - 20) (height - 20)
  color $ Hex 0x4c566a
  setLineWidth 1
  bezier 0 0 12 76 97 48 100 100

  circle 200 200 10
  point 220 220

  mapM_ (uncurry perlin) canvasPoints

  restore
