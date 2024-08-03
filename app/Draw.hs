module Draw (drawCanvas, canvasWidth, canvasHeight) where

import qualified GI.Gtk as Gtk
import GI.Cairo.Render hiding (x, y, width, height)
import Draw.Shape
import Draw.Color
import System.Random
import Control.Monad

canvasWidth :: Int
canvasWidth = 600

canvasHeight :: Int
canvasHeight = 600

-- randomCircle :: (RandomGen g) => g -> Render ()
randomCircle :: Render ()
randomCircle = do
    g <- getStdGen
    let (x, r1) = randomR (0.0, fromIntegral canvasWidth)  g
    let (y, r2) = randomR (0.0, fromIntegral canvasHeight) r1
    let (r, r3) = randomR (0.0, 100.0) r2
    let (red, r4)   = randomR (0.0, 1.0) r3
    let (green, r5) = randomR (0.0, 1.0) r4
    let (blue, r6)  = randomR (0.0, 1.0) r5
    setStdGen r6

    color $ RGB red green blue
    circle x y r

drawCanvas :: Gtk.IsWidget widget => widget -> Double -> Double -> Render ()
drawCanvas _canvas width height = do
    save

    color $ Hex 0xFFFFFF
    rect 0 0 width height

    replicateM_ 100 randomCircle

    restore
