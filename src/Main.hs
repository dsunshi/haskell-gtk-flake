{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

import Data.Maybe
import Data.Text as Text
import GI.Cairo.Render hiding (x, y, width, height)
import GI.Cairo.Render.Connector
import GI.GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Draw

drawCanvasHandler :: Gtk.IsWidget widget => widget -> Render Bool
drawCanvasHandler widget = do

  width  <- liftIO $ Gtk.widgetGetAllocatedWidth  widget
  height <- liftIO $ Gtk.widgetGetAllocatedHeight widget

  drawCanvas widget (fromIntegral width) (fromIntegral height)
  return True

main :: IO ()
main = do
  _ <- Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetPosition window Gtk.WindowPositionCenterAlways

  Gtk.widgetSetAppPaintable window True

  Gtk.windowSetDefaultSize window (fromIntegral canvasWidth)
                                  (fromIntegral canvasHeight)

  geometry <- Gdk.newZeroGeometry
  Gdk.setGeometryMaxWidth  geometry 512
  Gdk.setGeometryMaxHeight geometry 512
  Gdk.setGeometryMinWidth  geometry 32
  Gdk.setGeometryMinHeight geometry 32
  Gdk.setGeometryMinAspect geometry 1
  Gdk.setGeometryMaxAspect geometry 1

  Gtk.windowSetGeometryHints window (Just window) (Just geometry) []

  _ <- Gtk.onWidgetKeyPressEvent window $ \keyPressInfo -> do
    keyVal  <- Gdk.getEventKeyKeyval keyPressInfo
    keyName <- fromMaybe Text.empty <$> Gdk.keyvalName keyVal
    case Text.unpack keyName of
      "Escape" -> do Gtk.mainQuit
                     return True
      _        -> return False

  _ <- Gtk.onWidgetButtonPressEvent window $ \button -> do
    btnNo <- Gdk.getEventButtonButton button
    x     <- Gdk.getEventButtonX      button
    y     <- Gdk.getEventButtonY      button
    time  <- Gdk.getEventButtonTime   button
    case btnNo of
      -- left button
      1  -> do Gtk.windowBeginMoveDrag window 1 (round x) (round y) time
               return True
      -- middle button
      2  -> do Gtk.windowBeginResizeDrag window Gdk.WindowEdgeSouthEast 2 (round x) (round y) time
               return True
      _  -> return False

  canvas <- Gtk.drawingAreaNew

  Gtk.containerAdd       window canvas
  Gtk.setWindowDecorated window False
  Gtk.setWindowResizable window True
  Gtk.setWindowTitle     window (pack "Example Canvas")

  _ <- Gtk.onWidgetDraw canvas $ renderWithContext (drawCanvasHandler canvas)

  Gtk.widgetShowAll window

  _ <- timeoutAdd GI.GLib.PRIORITY_DEFAULT 1000 (Gtk.widgetQueueDraw window >> return True)

  Gtk.main
