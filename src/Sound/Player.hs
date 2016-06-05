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
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (def)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Process (ProcessHandle)

import Sound.Player.AudioInfo (SongInfo(SongInfo), fetchSongInfo)
import qualified Sound.Player.AudioPlay as AP (play, pause, resume, stop)
import Sound.Player.Types (Song(Song, songStatus), PlayerApp(PlayerApp,
  songsList, playerStatus, playback), Playback(Playback, playhead),
  Status(Play, Pause, Stop), PlayerEvent(VtyEvent, PlayheadAdvance))
import Sound.Player.Widgets (songWidget, playbackProgressBar)


-- | Draws application UI.
drawUI :: PlayerApp -> [Widget]
drawUI (PlayerApp l _ _ mPlayback)  = [ui]
  where
    label = str "Item " <+> cur <+> str " of " <+> total
    cur =
      case l ^. L.listSelectedL of
        Nothing -> str "-"
        Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ l ^. L.listElementsL
    box = B.borderWithLabel label $ L.renderList l (const songWidget)
    ui = vBox [ box
              , playbackProgressBar mPlayback l
              , str $ "Press enter to play/stop, spacebar to pause/resume, " ++
                      "left/right to play prev/next song, " ++
                      "q to exit."
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
appEvent app@(PlayerApp l status _ mPlayback) e =
  case e of
    -- press enter to play selected song, stop current song if playing
    VtyEvent (V.EvKey V.KEnter []) ->
      M.continue =<< stopAndPlaySelected app

    -- press spacebar to play/pause
    VtyEvent (V.EvKey (V.KChar ' ') []) ->
      case status of
        Play ->
          -- pause playing song
          case mPlayback of
            Nothing -> M.continue app
            Just (Playback playPos playProc _ _ _) -> do
              liftIO $ AP.pause playProc
              M.continue $ updateAppStatus app Pause playPos
        Pause ->
          -- resume playing song
          case mPlayback of
            Nothing -> M.continue app
            Just (Playback playPos playProc _ _ _) -> do
              liftIO $ AP.resume playProc
              M.continue $ updateAppStatus app Play playPos
        Stop ->
          -- play selected song
          M.continue =<< play (l ^. L.listSelectedL) app

    -- press left to play previous song
    VtyEvent (V.EvKey V.KLeft []) ->
      M.continue =<< stopAndPlayDelta (-1) app

    -- press right to play next song
    VtyEvent (V.EvKey V.KRight []) ->
      M.continue =<< stopAndPlayDelta 1 app

    -- press q to quit
    VtyEvent (V.EvKey (V.KChar 'q') []) ->
      M.halt =<< stop app

    -- any other event
    VtyEvent ev -> do
      l' <- handleEvent ev l
      M.continue app { songsList = l' }

    -- playhead advance event
    PlayheadAdvance ->
      M.continue =<< case status of
        Play ->
          case mPlayback of
            Nothing -> return app
            Just pb@(Playback _ _ ph _ _) ->
              if ph > 0 then
                -- advance playhead
                return app { playback = Just pb { playhead = ph - 1.0 } }
              else
                stopAndPlayDelta 1 app
        _ -> return app


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
stop :: (MonadIO m) => PlayerApp -> m PlayerApp
stop app@(PlayerApp _ _ _ Nothing) = return app
stop app@(PlayerApp _ _ _ (Just pb@(Playback playPos _ _ _ _))) = do
  liftIO $ stopPlayingSong pb
  return (updateAppStatus app Stop playPos) { playback = Nothing }
  where
    stopPlayingSong (Playback _ playProc _ _ threadId) = do
      AP.stop playProc
      killThread threadId


-- | Fetches song info, plays it, and starts a thread to advance the playhead.
play :: (MonadIO m) => Maybe Int -> PlayerApp -> m PlayerApp
play Nothing app = return app
play (Just _) app@(PlayerApp _ _ _ (Just _)) = return app
play (Just pos) app@(PlayerApp l _ chan _) = do
    (proc, duration, tId) <- liftIO $ playSong song
    return (updateAppStatus app Play pos) {
        playback = Just (Playback pos proc duration duration tId)
      }
  where
    songs = L.listElements l
    song = songs Vec.! pos
    failSongInfo :: SomeException -> IO SongInfo
    failSongInfo _ = return $ SongInfo (-1)
    playSong :: Song -> IO (ProcessHandle, Double, ThreadId)
    playSong (Song _ path _) = do
      musicDir <- defaultMusicDirectory
      (SongInfo duration) <- catch
        (fetchSongInfo $ musicDir </> path)
        failSongInfo
      proc <- AP.play $ musicDir </> path
      tId <- playheadAdvanceLoop chan
      return (proc, duration, tId)


-- Stops current song and play selected song.
stopAndPlaySelected :: (MonadIO m) => PlayerApp -> m PlayerApp
stopAndPlaySelected app = stop app >>= play mPos
  where
    mPos = songsList app ^. L.listSelectedL


-- Stops current song and play current song pos + delta.
stopAndPlayDelta :: (MonadIO m) => Int -> PlayerApp -> m PlayerApp
stopAndPlayDelta _ app@(PlayerApp _ _ _ Nothing) = return app
stopAndPlayDelta delta app@(PlayerApp l _ _ (Just (Playback playPos _ _ _ _))) =
    stop app >>= play (Just pos)
  where
    pos = (playPos + delta) `mod` Vec.length (L.listElements l)


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
