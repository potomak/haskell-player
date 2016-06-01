module Sound.Player.Widgets (
  songWidget
) where

import Brick.Types (Widget)
import Brick.Widgets.Core ((<+>), str, fill, vLimit)

import Sound.Player.Types (Song(Song), Status(Play, Pause))


songWidget :: Song -> Widget
songWidget (Song _ path status) =
    vLimit 1 $ str (statusSymbol status) <+> str " " <+> str path <+> fill ' '
  where
    statusSymbol Play = "♫"
    statusSymbol Pause = "•"
    statusSymbol _ = " "
