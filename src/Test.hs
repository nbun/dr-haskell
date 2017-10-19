module Test where

import System.Process
import System.FilePath
import Paths_drhaskell

test = do
  datadir <- getDataDir
  let path = datadir </> "Sounds" </> "sad_trombone.wav"
  createProcess (proc "aplay" [path])
