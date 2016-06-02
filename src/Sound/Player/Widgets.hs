{-# LANGUAGE OverloadedStrings #-}

module Sound.Player.Widgets (
  songWidget,
  helpDialog
) where

import Brick.Types (Widget)
import Brick.Widgets.Core ((<+>), str, fill, vLimit, padAll)
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D

import Sound.Player.Types (Song(Song), Status(Play, Pause))


songWidget :: Song -> Widget
songWidget (Song _ path status) =
    vLimit 1 $ str (statusSymbol status) <+> str " " <+> str path <+> fill ' '
  where
    statusSymbol Play = "♫"
    statusSymbol Pause = "►"
    statusSymbol _ = " "


helpDialog :: Widget
helpDialog =
    D.renderDialog d $ C.hCenter $ padAll 2 $ str "This is the dialog body."
  where
    d = D.dialog "helpDialog" (Just "Help") Nothing 60
