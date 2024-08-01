module Draw (drawCanvas, canvasWidth, canvasHeight) where

import qualified GI.Gtk as Gtk
import GI.Cairo.Render hiding (x, y, width, height)
import Draw.Shape
import Draw.Color
import Draw.Path

canvasWidth :: Int
canvasWidth = 400

canvasHeight :: Int
canvasHeight = 400

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

  restore
