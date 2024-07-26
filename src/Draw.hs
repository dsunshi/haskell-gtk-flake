{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Draw (drawCanvas, canvasWidth, canvasHeight) where

import qualified GI.Gtk as Gtk
import GI.Cairo.Render hiding (x, y, width, height)
import Draw.Shape
import Draw.Color

canvasWidth :: Int
canvasWidth = 256

canvasHeight :: Int
canvasHeight = 256

lerp :: Double -> -- x1
        Double -> -- y1
        Double -> -- x2
        Double -> -- y2
        Double -> -- t
        (Double, Double)
lerp x1 y1 x2 y2 t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)

quadratic :: Double -> -- x1
             Double -> -- y1
             Double -> -- x2
             Double -> -- y2
             Double -> -- x3
             Double -> -- y3
             Double -> -- t
             (Double, Double)
quadratic x1 y1 x2 y2 x3 y3 t = lerp p0x p0y p1x p1y t
    where
        (p0x, p0y) = lerp x1 y1 x2 y2 t
        (p1x, p1y) = lerp x2 y2 x3 y3 t

cubic :: Double -> -- x1
         Double -> -- y1
         Double -> -- x2
         Double -> -- y2
         Double -> -- x3
         Double -> -- y3
         Double -> -- x4
         Double -> -- y4
         Double -> -- t
         (Double, Double)
cubic x1 y1 x2 y2  x3 y3 x4 y4 t = lerp p0x p0y p1x p1y t
    where
        (p0x, p0y) = quadratic x1 y1 x2 y2 x3 y3 t
        (p1x, p1y) = quadratic x2 y2 x3 y3 x4 y4 t

bezier :: Double -> -- x1
          Double -> -- y1
          Double -> -- x2
          Double -> -- y2
          Double -> -- x3
          Double -> -- y3
          Double -> -- x4
          Double -> -- y4
          Render ()
bezier x1 y1 x2 y2  x3 y3 x4 y4 = do
    moveTo x1 y1
    mapM_ (uncurry lineTo) points
    stroke
    where
        points = map (cubic x1 y1 x2 y2  x3 y3 x4 y4) ts
        ts     = map (* res) [ 1 .. (1 / res)] :: [Double]
        res    = 0.05

drawCanvas :: Gtk.IsWidget widget => widget -> Double -> Double -> Render ()
drawCanvas _canvas width height = do
  save

  color $ Hex 0x4c566a
  rect 0 0 width height

  color $ Hex 0x2e3440
  rect 10 10 (width - 20) (height - 20)

  restore

  color $ Hex 0x4c566a
  setLineWidth 1
  curveTo 50 50 50 150 225 225
  stroke
  bezier 0 0 12 76 97 48 100 100
