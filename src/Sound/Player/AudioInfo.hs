{-# LANGUAGE OverloadedStrings #-}

module Sound.Player.AudioInfo (
  SongInfo(..),
  fetchSongInfo
) where

import Control.Exception (SomeException(SomeException), Exception)
import Data.ByteString.Lazy (ByteString, hGetContents)
import qualified Data.Text as T
import System.Process (StdStream(CreatePipe), CreateProcess(std_out),
  createProcess, proc)
import Text.Read (readMaybe)
import Text.XML (def, parseLBS)
import Text.XML.Cursor (($//), (&//), fromDocument, content, element)


data SongInfo = SongInfo {
    duration :: Double
  } deriving (Show)


newtype SongInfoParsingException = SongInfoParsingException String

instance Exception SongInfoParsingException

instance Show SongInfoParsingException where
  showsPrec _ (SongInfoParsingException err) = showString err


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
    maybe
      (Left . SomeException $ parsingException)
      (Right . SongInfo)
      (readFirst $ durations doc)
  where
    parsingException = SongInfoParsingException "Can't find song duration"
    durations doc = fromDocument doc $// durationElement &// content
    readFirst :: [T.Text] -> Maybe Double
    readFirst (d:_) = readMaybe (T.unpack d)
    readFirst _ = Nothing
    durationElement =
      element "{http://apple.com/core_audio/audio_info}duration"
