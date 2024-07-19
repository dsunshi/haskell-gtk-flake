{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}
{- cabal:
build-depends: base >= 4.16, haskell-gi-base, gi-gtk == 4.0.*
-}
import Control.Monad (void)

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import GI.Cairo.Render
import GI.Cairo.Render.Connector
import GI.GLib
import Data.Maybe
import Data.Text as Text
-- import Data.GI.Base

-- activate :: Gtk.Application -> IO ()
-- activate app = do
--   button <- new Gtk.Button [#label := "Click me",
--                             On #clicked (?self `set` [#sensitive := False,
--                                                       #label := "Thanks for clicking me"])]

--   window <- new Gtk.ApplicationWindow [#application := app,
--                                        #title := "Hi there",
--                                        #child := button]
--   window.show

initialSize :: Int
initialSize = 256


drawClockBackground :: Gtk.IsWidget widget => widget -> Bool -> Render ()
drawClockBackground canvas quality = do

  width  <- liftIO $ Gtk.widgetGetAllocatedWidth  canvas
  height <- liftIO $ Gtk.widgetGetAllocatedHeight canvas
  save
  scale (fromIntegral width) (fromIntegral height)

  setSourceRGB 0.78 0.82 0.805
  translate 0.5 0.5
  arc 0 0 (60/150) 0 (pi*2)
  fill

  restore


drawCanvasHandler :: Gtk.IsWidget widget => widget -> Render Bool
drawCanvasHandler widget = do
  drawClockBackground widget True
  return True

main :: IO ()
main = do
  Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel 
  Gtk.windowSetPosition window Gtk.WindowPositionCenterAlways

  Gtk.widgetSetAppPaintable window True

  Gtk.windowSetDefaultSize window (fromIntegral initialSize) 
                                  (fromIntegral initialSize)

  geometry <- Gdk.newZeroGeometry
  Gdk.setGeometryMaxWidth  geometry 512
  Gdk.setGeometryMaxHeight geometry 512
  Gdk.setGeometryMinWidth  geometry 32
  Gdk.setGeometryMinHeight geometry 32 
  Gdk.setGeometryMinAspect geometry 1
  Gdk.setGeometryMaxAspect geometry 1

  Gtk.windowSetGeometryHints window (Just window) (Just geometry) []

  Gtk.onWidgetKeyPressEvent window $ \keyPressInfo -> do 
    keyVal <- Gdk.getEventKeyKeyval keyPressInfo
    keyName <- fromMaybe Text.empty <$> Gdk.keyvalName keyVal
    case Text.unpack keyName of
      "Escape" -> do Gtk.mainQuit
                     return True
      _        -> return False

  Gtk.onWidgetButtonPressEvent window $ \button -> do
    btnNo <- Gdk.getEventButtonButton button
    x     <- Gdk.getEventButtonX button  
    y     <- Gdk.getEventButtonY button
    time  <- Gdk.getEventButtonTime button
    case btnNo of
      1  -> do Gtk.windowBeginMoveDrag window 1 (round x) (round y) time  -- left button
               return True
      2  -> do Gtk.windowBeginResizeDrag window Gdk.WindowEdgeSouthEast 2 -- middle button
                                         (round x) (round y) time 
               return True
      _  -> return False 

  canvas <- Gtk.drawingAreaNew
  Gtk.containerAdd window canvas 

  Gtk.setWindowDecorated window False
  Gtk.setWindowResizable window True
  Gtk.setWindowTitle window (pack "Cairo Clock")

  Gtk.onWidgetDraw canvas $ renderWithContext (drawCanvasHandler canvas) 

  Gtk.widgetShowAll window
  timeoutAdd GI.GLib.PRIORITY_DEFAULT 1000 (Gtk.widgetQueueDraw window >> return True)

  Gtk.main
