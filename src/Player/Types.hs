module Player.Types (
  PlayerApp(..),
  Song(..),
  Status(..)
) where

import Player.AudioInfo (SongInfo)


data PlayerApp = PlayerApp {
    songsList :: [Song],
    playerStatus :: Status,
    currentSong :: FilePath,
    currentPosition :: Double
  } deriving (Show)


data Song = Song {
    songInfo :: SongInfo,
    songPath :: FilePath
  } deriving (Show)


data Status = Play | Pause | Stop
  deriving (Show)
