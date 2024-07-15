{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}
{- cabal:
build-depends: base >= 4.16, haskell-gi-base, gi-gtk == 4.0.*
-}
import Control.Monad (void)

import qualified GI.Gtk as Gtk
import Data.GI.Base

activate :: Gtk.Application -> IO ()
activate app = do
  button <- new Gtk.Button [#label := "Click me",
                            On #clicked (?self `set` [#sensitive := False,
                                                      #label := "Thanks for clicking me"])]

  window <- new Gtk.ApplicationWindow [#application := app,
                                       #title := "Hi there",
                                       #child := button]
  window.show

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.example",
                              On #activate (activate ?self)]

  void $ app.run Nothing