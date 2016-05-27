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
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getEnv)
import System.FilePath ((</>))

import Player.AudioPlay (play, pause, resume, stop)
import Player.Types (Song(Song, songPath, songStatus), PlayerApp(PlayerApp,
  songsList, playerStatus, playback), Playback(Playback), Status(Play, Pause,
  Stop))
import Player.Widgets (songWidget)

drawUI :: PlayerApp -> [Widget]
drawUI (PlayerApp l _ _)  = [ui]
  where
    label = str "Item " <+> cur <+> str " of " <+> total
    cur =
      case l ^. L.listSelectedL of
        Nothing -> str "-"
        Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ l ^. L.listElementsL
    box = B.borderWithLabel label $ L.renderList l (const songWidget)
    ui = vBox [ box
              , str "Press spacebar to play/pause."
              , str "Press q to exit."
              ]

appEvent :: PlayerApp -> V.Event -> EventM (Next PlayerApp)
appEvent app@(PlayerApp l status mPlayback) e =
  case e of
    -- press spacebar to play/pause
    V.EvKey (V.KChar ' ') [] -> do
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
                Just (Playback playPos playProc _) -> do
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
                      liftIO $ stop playProc
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
                Just (Playback playPos playProc _) -> do
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
              proc <- liftIO $ do
                musicDir <- defaultMusicDirectory
                -- TODO: fetch song info
                -- songInfo <- mapM (fetchSongInfo . (musicDir </>)) paths
                play . (musicDir </>) $ songPath selectedSong
              M.continue app {
                  songsList = L.listReplace songs' (Just pos) l,
                  playerStatus = Play,
                  playback = Just (Playback pos proc 0.0)
                }
    -- press q to quit
    V.EvKey (V.KChar 'q') [] ->
      -- TODO: stop any current playing process
      M.halt app
    -- any other event
    ev -> do
      l' <- handleEvent ev l
      M.continue app { songsList = l' }


initialState :: IO PlayerApp
initialState = do
  paths <- listMusicDirectory
  let songs = map (\p -> Song Nothing p Stop) paths
      listWidget = L.list (Name "list") (Vec.fromList songs) 1
  return $ PlayerApp listWidget Stop Nothing


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr,         V.white `on` V.blue)
  , (L.listSelectedAttr, V.blue `on` V.white)
  ]


theApp :: M.App PlayerApp V.Event
theApp =
  M.App { M.appDraw = drawUI
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return
        , M.appAttrMap = const theMap
        , M.appLiftVtyEvent = id
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
appMain = M.defaultMain theApp =<< initialState
