module Draw (drawCanvas, canvasWidth, canvasHeight) where

import qualified GI.Gtk as Gtk
import GI.Cairo.Render hiding (x, y, width, height)
import Draw.Shape
import Draw.Color

canvasWidth :: Int
canvasWidth = 600

canvasHeight :: Int
canvasHeight = 600

drawCanvas :: Gtk.IsWidget widget => widget -> Double -> Double -> Render ()
drawCanvas _canvas width height = do
    save

    color $ Hex 0xFFFFFF
    rect 0 0 width height

    restore
