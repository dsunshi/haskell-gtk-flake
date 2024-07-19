module Draw (drawCanvas, canvasWidth, canvasHeight) where

import qualified GI.Gtk as Gtk
import GI.Cairo.Render

canvasWidth :: Int
canvasWidth = 256

canvasHeight :: Int
canvasHeight = 256

drawCanvas :: Gtk.IsWidget widget => widget -> Render ()
drawCanvas canvas = do

  width  <- liftIO $ Gtk.widgetGetAllocatedWidth  canvas
  height <- liftIO $ Gtk.widgetGetAllocatedHeight canvas

  save

  scale (fromIntegral width) (fromIntegral height)

  setSourceRGB 0.78 0.82 0.805
  translate 0.5 0.5
  arc 0 0 (60/150) 0 (pi*2)
  fill

  restore
