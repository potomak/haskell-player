module Sound.Player.Widgets (
  songWidget,
  playbackProgressBar
) where

import Brick.Types (Widget)
import Brick.Widgets.Core ((<+>), str, fill, vLimit, vBox)
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P
import qualified Data.Vector as Vec
import GHC.Float (double2Float)
import Lens.Micro ((^.))

import Sound.Player.Types (Song(Song), Status(Play, Pause), Playback(Playback))


-- | A song 'Widget', one of the items in the songs list.
songWidget :: Song -> Widget
songWidget (Song _ path status) =
    vLimit 1 $ str (statusSymbol status) <+> str " " <+> str path <+> fill ' '
  where
    statusSymbol Play = "♫"
    statusSymbol Pause = "•"
    statusSymbol _ = " "


-- | A 'Widget' that shows two progress bars, the top bar shows the path of
-- the playing song, the bottom bar shows the playhead position, song duration
-- and playback percentage.
playbackProgressBar :: Maybe Playback -> L.List Song -> Widget
playbackProgressBar mPlayback l =
  vBox [ titlePlaybackProgressBar mPlayback l
       , infoPlaybackProgressBar mPlayback
       ]


-- | A 'Widget' that shows a progress bar with the path of the song.
titlePlaybackProgressBar :: Maybe Playback -> L.List Song -> Widget
titlePlaybackProgressBar Nothing _ = str " "
titlePlaybackProgressBar (Just pb@(Playback playPos _ _ _ _)) l =
    P.progressBar (Just path) (playbackProgress pb)
  where
    songs = l ^. L.listElementsL
    (Song _ path _) = songs Vec.! playPos


-- | A 'Widget' that shows a progress bar with the playhead position, song
-- duration and playback percentage.
infoPlaybackProgressBar :: Maybe Playback -> Widget
infoPlaybackProgressBar Nothing = str " "
infoPlaybackProgressBar (Just pb@(Playback _ _ ph d _)) =
    P.progressBar (Just title) progress
  where
    progress = playbackProgress pb
    percentage :: Integer
    percentage = round (progress * 100)
    title =
      formatSeconds (d - ph) ++ " / " ++
      formatSeconds d ++ " ~ " ++
      show percentage ++ "%"


-- | A 'Float' number between 0 and 1 that is playing song's progress.
playbackProgress :: Playback -> Float
playbackProgress (Playback _ _ ph d _) = 1 - (double2Float ph / double2Float d)


-- | Returns a string that is a time formatted as /mm:ss/
formatSeconds :: Double -> String
formatSeconds s = pad (minutes s) ++ ":" ++ pad (seconds s)
  where
    seconds :: Double -> Int
    seconds n = round n `mod` 60
    minutes :: Double -> Int
    minutes n = round n `div` 60
    pad :: Int -> String
    pad n
        | length ns < 2 = "0" ++ ns
        | otherwise = ns
      where
        ns = show n
