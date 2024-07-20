
module Draw.Shape (rect) where

import GI.Cairo.Render hiding (x, y, width, height)

rect :: Double -> Double -> Double -> Double -> Render ()
rect x y w h = do
    rectangle x y w h
    fill
