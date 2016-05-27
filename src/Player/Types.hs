module Player.Types (
  PlayerApp(..),
  Playback(..),
  Song(..),
  Status(..)
) where

import Brick.Widgets.List (List)
import System.Process (ProcessHandle)

import Player.AudioInfo (SongInfo)


data PlayerApp = PlayerApp {
    songsList :: List Song,
    playerStatus :: Status,
    playback :: Maybe Playback
  }


data Playback = Playback {
    position :: Int,
    process :: ProcessHandle,
    playhead :: Double
  }


data Song = Song {
    songInfo :: Maybe SongInfo,
    songPath :: FilePath,
    songStatus :: Status
  } deriving (Show)


data Status = Play | Pause | Stop
  deriving (Show)
