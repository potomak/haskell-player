-- TODO: playlist
-- TODO: search
-- TODO: go to playing song
-- TODO: next/previous
-- TODO: help dialog

{-# LANGUAGE OverloadedStrings #-}

module Sound.Player (
  appMain
) where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget, EventM, Next, Name(Name), handleEvent)
import Brick.Widgets.Core ((<+>), str, vBox)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P
import Brick.Util (on)
import Control.Concurrent (Chan, ThreadId, forkIO, killThread, newChan,
  writeChan, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import GHC.Float (double2Float)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Process (ProcessHandle)

import Sound.Player.AudioInfo (SongInfo(SongInfo), fetchSongInfo)
import Sound.Player.AudioPlay (play, pause, resume, stop)
import Sound.Player.Types (Song(Song, songStatus), PlayerApp(PlayerApp, songsList,
  playerStatus, playback), Playback(Playback, playhead), Status(Play, Pause,
  Stop), PlayerEvent(VtyEvent, PlayheadAdvance))
import Sound.Player.Widgets (songWidget)


-- | Draws application UI.
drawUI :: PlayerApp -> [Widget]
drawUI (PlayerApp l _ _ mPlayback)  = [ui]
  where
    playheadWidget Nothing = str " "
    playheadWidget (Just pb@(Playback _ _ ph d _)) = str $
      "Duration: " ++ show d ++
      " - Position: " ++ show (d - ph) ++
      " - Progress: " ++ show (playheadProgress pb)
    playheadProgressBar Nothing = str " "
    playheadProgressBar (Just pb) =
      P.progressBar Nothing (playheadProgress pb)
    -- A number between 0 and 1 that is current song's progress
    playheadProgress (Playback _ _ ph d _) =
      1 - (double2Float ph / double2Float d)
    label = str "Item " <+> cur <+> str " of " <+> total
    cur =
      case l ^. L.listSelectedL of
        Nothing -> str "-"
        Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ l ^. L.listElementsL
    box = B.borderWithLabel label $ L.renderList l (const songWidget)
    ui = vBox [ box
              , playheadProgressBar mPlayback
              , playheadWidget mPlayback
              , str "Press enter to play/stop, spacebar to pause/resume, q to exit."
              ]


-- Updates the selected song status and the song list in the app state.
updateAppStatus :: PlayerApp -> Status -> Int -> PlayerApp
updateAppStatus app@(PlayerApp l _ _ _) status pos =
    app {
        songsList = L.listReplace songs' mPos l,
        playerStatus = status
      }
  where
    songs = L.listElements l
    mPos = l ^. L.listSelectedL
    song = songs Vec.! pos
    songs' = songs Vec.// [(pos, song { songStatus = status })]


-- | App events handler.
appEvent :: PlayerApp -> PlayerEvent -> EventM (Next PlayerApp)
appEvent app@(PlayerApp l status chan mPlayback) e =
  case e of
    -- play selected song, stop current song if playing
    VtyEvent (V.EvKey V.KEnter []) -> do
      let mPos = l ^. L.listSelectedL
          songs = L.listElements l
      app' <- case status of
        Stop -> return app
        _ -> case mPlayback of
          Nothing -> return app
          Just pb@(Playback playPos _ _ _ _) -> do
            liftIO $ stopPlayingSong pb
            return (updateAppStatus app Stop playPos) { playback = Nothing }
      case mPos of
        Nothing -> M.continue app'
        Just pos -> do
          let song = songs Vec.! pos
          (proc, duration, tId) <- liftIO $ playSong song chan
          M.continue (updateAppStatus app' Play pos) {
              playback = Just (Playback pos proc duration duration tId)
            }

    -- press spacebar to play/pause
    VtyEvent (V.EvKey (V.KChar ' ') []) -> do
      let mPos = l ^. L.listSelectedL
          songs = L.listElements l
      case status of
        Play ->
          -- pause playing song
          case mPlayback of
            Nothing -> M.continue app
            Just (Playback playPos playProc _ _ _) -> do
              liftIO $ pause playProc
              M.continue $ updateAppStatus app Pause playPos
        Pause ->
          -- resume playing song
          case mPlayback of
            Nothing -> M.continue app
            Just (Playback playPos playProc _ _ _) -> do
              liftIO $ resume playProc
              M.continue $ updateAppStatus app Play playPos
        Stop ->
          -- play selected song
          case mPos of
            Nothing -> M.continue app
            Just pos -> do
              let song = songs Vec.! pos
              (proc, duration, tId) <- liftIO $ playSong song chan
              M.continue (updateAppStatus app Play pos) {
                  playback = Just (Playback pos proc duration duration tId)
                }
    -- press q to quit
    VtyEvent (V.EvKey (V.KChar 'q') []) -> do
      -- stop current process if present
      maybe (return ()) (liftIO . stopPlayingSong) mPlayback
      M.halt app

    -- any other event
    VtyEvent ev -> do
      l' <- handleEvent ev l
      M.continue app { songsList = l' }

    -- playhead advance event
    PlayheadAdvance ->
      case status of
        Play ->
          case mPlayback of
            Nothing -> M.continue app
            Just pb@(Playback playPos _ ph _ _) ->
              if ph > 0 then
                -- advance playhead
                M.continue app { playback = Just pb { playhead = ph - 1.0 } }
              else do
                let songs = L.listElements l
                    nextPos = (playPos + 1) `mod` Vec.length songs
                    nextSong = songs Vec.! nextPos
                -- stop current song
                liftIO $ stopPlayingSong pb
                -- play next song
                (proc, duration, tId) <- liftIO $ playSong nextSong chan
                M.continue (updateAppStatus (updateAppStatus app Stop playPos) Play nextPos) {
                    playback = Just (Playback nextPos proc duration duration tId)
                  }
        _ -> M.continue app


-- | Forks a thread that will trigger a 'Types.PlayheadAdvance' event every
-- second.
playheadAdvanceLoop :: Chan PlayerEvent -> IO ThreadId
playheadAdvanceLoop chan = forkIO loop
  where
    loop = do
      threadDelay 1000000
      writeChan chan PlayheadAdvance
      loop


-- | Stops the song that is currently playing and kills the playback thread.
stopPlayingSong :: Playback -> IO ()
stopPlayingSong (Playback _ playProc _ _ threadId) = do
  stop playProc
  killThread threadId


-- | Fetches song info, plays it, and starts a thread to advance the playhead.
playSong :: Song -> Chan PlayerEvent -> IO (ProcessHandle, Double, ThreadId)
playSong (Song _ path _) chan = do
  musicDir <- defaultMusicDirectory
  (SongInfo duration) <- fetchSongInfo $ musicDir </> path
  proc <- play $ musicDir </> path
  tId <- playheadAdvanceLoop chan
  return (proc, duration, tId)


-- | Returns the initial state of the application.
initialState :: IO PlayerApp
initialState = do
  chan <- newChan
  paths <- listMusicDirectory
  let songs = map (\p -> Song Nothing p Stop) paths
      listWidget = L.list (Name "list") (Vec.fromList songs) 1
  return $ PlayerApp listWidget Stop chan Nothing


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr,             V.white `on` V.blue)
  , (L.listSelectedAttr,     V.blue `on` V.white)
  , (P.progressCompleteAttr, V.blue `on` V.white)
  ]


theApp :: M.App PlayerApp PlayerEvent
theApp =
  M.App { M.appDraw = drawUI
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return
        , M.appAttrMap = const theMap
        , M.appLiftVtyEvent = VtyEvent
        }


-- TODO: return only audio files
-- | Returns the list of files in the default music directory.
listMusicDirectory :: IO [FilePath]
listMusicDirectory = do
    musicDir <- defaultMusicDirectory
    map (stripMusicDirectory musicDir) <$> listMusicDirectoryRic [musicDir]
  where
    listMusicDirectoryRic [] = return []
    listMusicDirectoryRic (p:ps) = do
      isDirectory <- doesDirectoryExist p
      if isDirectory
        then do
          files <- map (p </>) . filter visible <$> getDirectoryContents p
          listMusicDirectoryRic (files ++ ps)
        else do
          files <- listMusicDirectoryRic ps
          return $ p:files
    visible = not . isPrefixOf "."
    stripMusicDirectory musicDir = fromMaybe musicDir . stripPrefix musicDir


-- | The default music directory is /$HOME\/Music/.
defaultMusicDirectory :: IO FilePath
defaultMusicDirectory = (</> "Music/") <$> getEnv "HOME"


appMain :: IO PlayerApp
appMain = do
  playerApp@(PlayerApp _ _ chan _) <- initialState
  M.customMain (V.mkVty def) chan theApp playerApp
