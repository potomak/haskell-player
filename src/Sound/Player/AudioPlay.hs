module Sound.Player.AudioPlay (
  play,
  pause,
  resume,
  stop
) where

import System.Process (ProcessHandle, StdStream(CreatePipe),
  CreateProcess(std_err), createProcess, proc, terminateProcess)
import System.Posix (ProcessID)
import System.Posix.Signals (signalProcess)
import System.Process.Internals (ProcessHandle__(OpenHandle, ClosedHandle),
  withProcessHandle)


-- | Creates an @afplay@ process to play file at @path@.
--
-- Note: it suppresses process' @stderr@.
play :: FilePath -> IO ProcessHandle
play path = do
  (_, _, _, processHandle) <-
    -- TODO: update System.Process version to use NoStream
    createProcess (proc "afplay" [path]) {
        std_err = CreatePipe
      }
  return processHandle


-- | Sends a @17@ signal to @ph@'s process. If @ph@ is the handle of a running
-- @afplay@ process it will pause playback.
pause :: ProcessHandle -> IO ()
pause ph =
  maybe (return ()) (signalProcess 17) =<< getPid ph


-- | Sends a @19@ signal to @ph@'s process. If @ph@ is the handle of a running
-- @afplay@ process it will resume playback.
resume :: ProcessHandle -> IO ()
resume ph =
  maybe (return ()) (signalProcess 19) =<< getPid ph


-- | Terminates the selected process. If @ph@ is the handle of a running
-- @afplay@ process it will stop playback.
stop :: ProcessHandle -> IO ()
stop = terminateProcess


-- See https://mail.haskell.org/pipermail/haskell-cafe/2012-October/104028.html
getPid :: ProcessHandle -> IO (Maybe ProcessID)
getPid ph = withProcessHandle ph (return . go)
  where
    go (OpenHandle pid) = Just pid
    go (ClosedHandle _) = Nothing
