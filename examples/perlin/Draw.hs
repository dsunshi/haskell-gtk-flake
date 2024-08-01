{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Draw (drawCanvas, canvasWidth, canvasHeight) where

import qualified GI.Gtk as Gtk
import GI.Cairo.Render hiding (x, y, width, height)
import Draw.Shape
import Draw.Color
import Draw.Noise

-- Set the width of the window
canvasWidth :: Int
canvasWidth = 500

-- Set the Height of the window
canvasHeight :: Int
canvasHeight = 500

-- Draws a point representing the grayscale perlin noise value
--
-- This function takes 2 arguments:
-- x: the x position in the canvas
-- y: the y position in the canvas
--
-- For a given (x, y) coordinate the noise function is used to get the perlin noise at that (x, y) pair.
-- The result is used as a grayscale color value, where a point is then drawn.
perlin :: Double -> Double -> Render ()
perlin x y = do
    color $ Gray ((1.0 + noise x y 0) / 2.0) -- [-1 .. 1] -> [0 .. 1]
    point x y

-- Generate the set of all (x, y) pairs baesd on the size of our canvas
canvasPoints :: [(Double, Double)]
canvasPoints = [(x, y)
  | x <- [1.0 .. fromIntegral canvasWidth]
  , y <- [1.0 .. fromIntegral canvasHeight] ]

-- This function is automatically called to continously update the window
drawCanvas :: Gtk.IsWidget widget => widget -> Double -> Double -> Render ()
drawCanvas _canvas width height = do
  save

  -- Draw a background. Try removing this to see what happens!
  color $ Hex 0x000000
  rect 0 0 width height

  -- Call our draw function for each point in the canvas
  mapM_ (uncurry perlin) canvasPoints

  restore
