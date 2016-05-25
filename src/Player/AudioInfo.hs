{-# LANGUAGE OverloadedStrings #-}

module Player.AudioInfo (
  SongInfo(..),
  fetchSongInfo
) where

import Control.Exception (SomeException)
import Data.ByteString.Lazy (ByteString, hGetContents)
import qualified Data.Text as T
import System.Process (StdStream(CreatePipe), CreateProcess(std_out),
  createProcess, proc)
import Text.XML (def, parseLBS)
import Text.XML.Cursor (($//), (&//), fromDocument, content, element)


data SongInfo = SongInfo {
    duration :: Double
  } deriving (Show)


fetchSongInfo :: FilePath -> IO SongInfo
fetchSongInfo path = do
  songInfoXML <- fetchRawSongInfo path
  case parseSongInfo songInfoXML of
    Left _ -> fail "Song info parsing error"
    Right songInfo -> return songInfo


fetchRawSongInfo :: FilePath -> IO ByteString
fetchRawSongInfo path = do
  (_, Just hout, _, _) <-
    createProcess (proc "afinfo" ["-x", path]) {
        std_out = CreatePipe
      }
  hGetContents hout


parseSongInfo :: ByteString -> Either SomeException SongInfo
parseSongInfo contents = do
    doc <- parseLBS def contents
    let durations = fromDocument doc $// durationElement &// content
    return . SongInfo . read . T.unpack . T.concat $ durations
  where
    durationElement =
      element "{http://apple.com/core_audio/audio_info}duration"
