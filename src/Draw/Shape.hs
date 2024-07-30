
module Draw.Shape (rect, circle, point) where

import GI.Cairo.Render hiding (x, y, width, height)

rect :: Double -> Double -> Double -> Double -> Render ()
rect x y w h = do
    rectangle x y w h
    fill

circle :: Double -> Double -> Double -> Render ()
circle x y d = do
    arc x y (d / 2) 0 (2 * pi)
    fill

point :: Double -> Double -> Render ()
point x y = circle x y 1
