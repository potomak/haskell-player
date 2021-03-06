module Sound.Player.Types (
  PlayerApp(..),
  Playback(..),
  Song(..),
  Status(..),
  PlayerEvent(..)
) where

import Brick.Widgets.List (List)
import Control.Concurrent (ThreadId, Chan)
import qualified Graphics.Vty as V
import System.Process (ProcessHandle)

import Sound.Player.AudioInfo (SongInfo)


data PlayerApp = PlayerApp {
    songsList :: List Song,
    playerStatus :: Status,
    playbackChan :: Chan PlayerEvent,
    playback :: Maybe Playback
  }


data Playback = Playback {
    position :: Int,
    process :: ProcessHandle,
    playhead :: Double,
    duration :: Double,
    playheadThread :: ThreadId
  }


data Song = Song {
    songInfo :: Maybe SongInfo,
    songPath :: FilePath,
    songStatus :: Status
  } deriving (Show)


data Status = Play | Pause | Stop
  deriving (Show)


data PlayerEvent = VtyEvent V.Event | PlayheadAdvance
