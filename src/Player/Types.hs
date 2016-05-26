module Player.Types (
  PlayerApp(..),
  Song(..),
  Status(..)
) where

import Brick.Widgets.List (List)

import Player.AudioInfo (SongInfo)


data PlayerApp = PlayerApp {
    songsList :: List Song,
    playerStatus :: Status,
    currentSong :: Maybe Song,
    currentPosition :: Double
  }


data Song = Song {
    songInfo :: SongInfo,
    songPath :: FilePath
  } deriving (Show)


data Status = Play | Pause | Stop
  deriving (Show)
