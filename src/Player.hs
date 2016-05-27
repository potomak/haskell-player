-- TODO: playlist
-- TODO: play next song
-- TODO: search

{-# LANGUAGE OverloadedStrings #-}

module Player (
  appMain
) where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget, EventM, Next, Name(Name), handleEvent)
import Brick.Widgets.Core ((<+>), str, vBox)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import Brick.Util (on)
import Control.Concurrent (forkIO, killThread, newChan, writeChan, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getEnv)
import System.FilePath ((</>))

import Player.AudioInfo (SongInfo(SongInfo), fetchSongInfo)
import Player.AudioPlay (play, pause, resume, stop)
import Player.Types (Song(Song, songPath, songStatus), PlayerApp(PlayerApp,
  songsList, playerStatus, playback), Playback(Playback, playhead),
  Status(Play, Pause, Stop), PlayheadAdvance(VtyEvent, PlayheadAdvance))
import Player.Widgets (songWidget)

drawUI :: PlayerApp -> [Widget]
drawUI (PlayerApp l _ _ mPlayback)  = [ui]
  where
    durationWidget Nothing = str "Duration: "
    durationWidget (Just (Playback _ _ ph _)) = str $ "Duration: " ++ show ph
    label = str "Item " <+> cur <+> str " of " <+> total
    cur =
      case l ^. L.listSelectedL of
        Nothing -> str "-"
        Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ l ^. L.listElementsL
    box = B.borderWithLabel label $ L.renderList l (const songWidget)
    ui = vBox [ box
              , durationWidget mPlayback
              , str "Press spacebar to play/pause."
              , str "Press q to exit."
              ]

appEvent :: PlayerApp -> PlayheadAdvance -> EventM (Next PlayerApp)
appEvent app@(PlayerApp l status chan mPlayback) e =
  case e of
    -- press spacebar to play/pause
    VtyEvent (V.EvKey (V.KChar ' ') []) -> do
      let mPos = l ^. L.listSelectedL
          songs = L.listElements l
      case mPos of
        Nothing -> M.continue app
        Just pos -> do
          let selectedSong = songs Vec.! pos
          case status of
            Play ->
              -- pause/stop playing the selected song
              case mPlayback of
                Nothing -> M.continue app
                Just (Playback playPos playProc _ threadId) -> do
                  app' <- if playPos == pos
                    then do
                      let songs' = songs Vec.// [(pos, selectedSong { songStatus = Pause })]
                      liftIO $ pause playProc
                      return app {
                          songsList = L.listReplace songs' (Just pos) l,
                          playerStatus = Pause
                        }
                    else do
                      let song = songs Vec.! playPos
                          songs' = songs Vec.// [(playPos, song { songStatus = Stop })]
                      liftIO $ do
                        stop playProc
                        killThread threadId
                      return app {
                          songsList = L.listReplace songs' (Just pos) l,
                          playerStatus = Stop,
                          playback = Nothing
                        }
                  M.continue app'
            Pause ->
              -- TODO: resume/play the selected song
              -- if playing song is equal to the selected song
              --   resume
              -- else
              --   play
              case mPlayback of
                Nothing -> M.continue app
                Just (Playback playPos playProc _ _) -> do
                  app' <- do
                    let song = songs Vec.! playPos
                        songs' = songs Vec.// [(playPos, song { songStatus = Play })]
                    liftIO $ resume playProc
                    return app {
                        songsList = L.listReplace songs' (Just pos) l,
                        playerStatus = Play
                      }
                  M.continue app'
            Stop -> do
              let songs' = songs Vec.// [(pos, selectedSong { songStatus = Play })]
              (proc, duration, threadId) <- liftIO $ do
                  musicDir <- defaultMusicDirectory
                  (SongInfo duration) <- fetchSongInfo $ musicDir </> songPath selectedSong
                  proc <- play $ musicDir </> songPath selectedSong
                  threadId <- forkIO $
                    let
                      playheadAdvanceLoop = do
                        threadDelay 1000000
                        writeChan chan PlayheadAdvance
                        playheadAdvanceLoop
                    in playheadAdvanceLoop
                  return (proc, duration, threadId)
              M.continue app {
                  songsList = L.listReplace songs' (Just pos) l,
                  playerStatus = Play,
                  playback = Just (Playback pos proc (-duration) threadId)
                }
    -- press q to quit
    VtyEvent (V.EvKey (V.KChar 'q') []) ->
      -- TODO: stop any current playing process
      M.halt app
    -- any other event
    VtyEvent ev -> do
      l' <- handleEvent ev l
      M.continue app { songsList = l' }
    PlayheadAdvance ->
      case status of
        Play ->
          case mPlayback of
            Nothing -> M.continue app
            Just pb@(Playback playPos playProc p threadId) ->
              if p < 0
                then
                  -- advance playhead
                  M.continue app {
                      playback = Just pb { playhead = p + 1.0 }
                    }
                else do
                  -- TODO: check for bounds
                  let songs = L.listElements l
                      song = songs Vec.! playPos
                      nextPos = playPos + 1
                      nextSong = songs Vec.! nextPos
                      songs' = songs Vec.// [
                          (playPos, song { songStatus = Stop }),
                          (nextPos, nextSong { songStatus = Play })
                        ]
                  -- stop current song
                  liftIO $ do
                    stop playProc
                    killThread threadId
                  -- play next song
                  (proc, duration, tId) <- liftIO $ do
                      musicDir <- defaultMusicDirectory
                      -- TODO: fetch song info
                      (SongInfo duration) <- fetchSongInfo $ musicDir </> songPath nextSong
                      proc <- play $ musicDir </> songPath nextSong
                      tId <- forkIO $
                        let
                          playheadAdvanceLoop = do
                            threadDelay 1000000
                            writeChan chan PlayheadAdvance
                            playheadAdvanceLoop
                        in playheadAdvanceLoop
                      return (proc, duration, tId)
                  M.continue app {
                      songsList = L.listReplace songs' (Just playPos) l,
                      playback = Just (Playback nextPos proc (-duration) tId)
                    }
        _ -> M.continue app


initialState :: IO PlayerApp
initialState = do
  chan <- newChan
  paths <- listMusicDirectory
  let songs = map (\p -> Song Nothing p Stop) paths
      listWidget = L.list (Name "list") (Vec.fromList songs) 1
  return $ PlayerApp listWidget Stop chan Nothing


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr,         V.white `on` V.blue)
  , (L.listSelectedAttr, V.blue `on` V.white)
  ]


theApp :: M.App PlayerApp PlayheadAdvance
theApp =
  M.App { M.appDraw = drawUI
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return
        , M.appAttrMap = const theMap
        , M.appLiftVtyEvent = VtyEvent
        }


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


defaultMusicDirectory :: IO FilePath
defaultMusicDirectory = (</> "Music/") <$> getEnv "HOME"


appMain :: IO PlayerApp
appMain = do
  playerApp@(PlayerApp _ _ chan _) <- initialState
  M.customMain (V.mkVty def) chan theApp playerApp
