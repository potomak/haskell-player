module Player.Widgets (
  songWidget
) where

import Brick.Types (Widget)
import Brick.Widgets.Core ((<+>), str, fill, vLimit)

import Player.AudioInfo (SongInfo(duration))
import Player.Types (Song(Song))


songWidget :: Song -> Widget
songWidget (Song info path) =
    vLimit 1 $ str path <+> fill ' ' <+> str (humanDuration $ duration info)
  where
    secondsDuration :: Double -> Int
    secondsDuration d = round d `mod` 60
    minutesDuration :: Double -> Int
    minutesDuration d = round d `div` 60
    humanDuration :: Double -> String
    humanDuration d =
         show (minutesDuration d) ++ "m"
      ++ padSeconds (show (secondsDuration d)) ++ "s"
    padSeconds :: String -> String
    padSeconds s
      | length s < 2 = "0" ++ s
      | otherwise = s
