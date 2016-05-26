module Player.AudioPlay (
  playSong,
  pause,
  resume,
  stop
) where

import Control.Monad (void)
import Data.Maybe (fromJust)
import System.Process (ProcessHandle, createProcess, proc, terminateProcess)
import System.Process.Internals (ProcessHandle__(OpenHandle, ClosedHandle),
  PHANDLE, withProcessHandle)


playSong :: FilePath -> IO ProcessHandle
playSong path = do
  (_, _, _, processHandle) <- createProcess (proc "afplay" [path])
  return processHandle


pause :: ProcessHandle -> IO ()
pause ph = do
  mPid <- getPid ph
  void $ createProcess (proc "kill" ["-17", show $ fromJust mPid])


resume :: ProcessHandle -> IO ()
resume ph = do
  mPid <- getPid ph
  void $ createProcess (proc "kill" ["-19", show $ fromJust mPid])


stop :: ProcessHandle -> IO ()
stop = terminateProcess


-- See https://mail.haskell.org/pipermail/haskell-cafe/2012-October/104028.html
getPid :: ProcessHandle -> IO (Maybe PHANDLE)
getPid ph = withProcessHandle ph (return . go)
  where
    go (OpenHandle pid) = Just pid
    go (ClosedHandle _) = Nothing
