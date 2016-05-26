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
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Safe (headMay)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getEnv)
import System.FilePath ((</>))

import Player.AudioInfo (fetchSongInfo)
import Player.AudioPlay (playSong)
import Player.Types (Song(Song, songPath), PlayerApp(PlayerApp, songsList),
  Status(Play, Pause, Stop))
import Player.Widgets (songWidget)

drawUI :: PlayerApp -> [Widget]
drawUI (PlayerApp l _ _ _)  = [ui]
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
appEvent app@(PlayerApp l status _ _) e =
  case e of
    -- press spacebar to play/pause
    V.EvKey (V.KChar ' ') [] -> do
      case status of
        Play ->
          -- TODO: stop playing currentSong - terminateProcess
          return ()
        Pause ->
          -- TODO: resume playing currentSong at currentPosition
          return ()
        Stop -> do
          let mPos = l ^. L.listSelectedL
              songPaths = Vec.map songPath $ L.listElements l
          case mPos of
            Nothing -> return ()
            Just pos -> liftIO $ do
              musicDir <- defaultMusicDirectory
              void . playSong . (musicDir </>) $ songPaths Vec.! pos
      M.continue app
    -- press q to quit
    V.EvKey (V.KChar 'q') [] -> M.halt app
    -- any other event
    ev -> do
      l' <- handleEvent ev l
      M.continue app { songsList = l' }


initialState :: IO PlayerApp
initialState = do
  musicDir <- defaultMusicDirectory
  songPaths <- listMusicDirectory
  songInfos <- mapM (fetchSongInfo . (musicDir </>)) songPaths
  let songs = zipWith Song songInfos songPaths
      listWidget = L.list (Name "list") (Vec.fromList songs) 1
  return $ PlayerApp listWidget Stop (headMay songs) 0.0


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
