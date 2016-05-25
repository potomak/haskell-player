module Player.AudioPlay (
  playSong
) where

import GHC.IO.Handle (Handle)
import System.Process (ProcessHandle, createProcess, proc)


playSong :: FilePath -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
playSong path = createProcess (proc "afplay" [path])
