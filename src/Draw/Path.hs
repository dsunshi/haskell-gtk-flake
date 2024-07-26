
module Draw.Path (bezier) where

import GI.Cairo.Render hiding (x, y, width, height)

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
