{-# LANGUAGE ScopedTypeVariables #-}
module CodeCoverage.Coverage (module CodeCoverage.Coverage) where

import           Control.Exception
import           Language.Haskell.Exts
import           Paths_drhaskell
import           StaticAnalysis.Messages.ErrorToLint
import           System.Directory
import           System.IO
import           System.IO.Temp
import           System.Process
import           Trace.Hpc.Mix
import           Trace.Hpc.Util
import           Util.ModifyAst

getConverageOutput :: ModifiedModule -> IO [Lint]
getConverageOutput m = do
    let plainFile = printModified m
    tix <- (withSystemTempDirectory "drhaskell-lint." $ runInTempDir plainFile)
    case tix of
        Just t  -> return $ parseMix t
        Nothing -> return []

--Mix FilePath UTCTime Hash Int [MixEntry]
parseMix :: Mix -> [Lint]
parseMix (Mix _ _ _ _ mixpos) = loopMixPos mixpos
    where loopMixPos :: [MixEntry] -> [Lint] --type MixEntry = (HpcPos, BoxLabel)
          loopMixPos []            = []
          loopMixPos ((pos,_):mes) = Lint "HPC" (fromHpcPos pos) Warning "Covered" : loopMixPos mes -- data Lint = Lint Filename Position MessageClass Message

extractStartAndEndColumn :: [Integer] -> (Int, Int)
extractStartAndEndColumn [] = (0,0)
extractStartAndEndColumn xs = (fromIntegral $ head xs,
                               fromIntegral $ last xs)

runInTempDir :: String -> FilePath -> IO (Maybe Mix)
runInTempDir plainFile tmpDir = do
    setCurrentDirectory tmpDir
    datadir <- getDataDir
    writeFile (tmpDir ++ "/tmp.hs") plainFile
    tmpExists <- doesFileExist (tmpDir ++ "/tmp.hs")
    if tmpExists
    then do _ <- catch (readProcess "ghc" ["-fhpc", "tmp.hs", "-o", "tmp", "-idirs:" ++ datadir ++ "/TargetModules"] [])
                       (\(e :: IOException) -> do return "")
            tmpCompExists <- doesFileExist (tmpDir ++ "/tmp")
            if tmpCompExists
            then do tmpout <- readProcess "./tmp" [] []
                    hpcDirExists <- doesDirectoryExist (tmpDir ++ "/.hpc")
                    if hpcDirExists
                    then do mix <- readMix [(tmpDir ++ "/.hpc")] (Left "Main")
                            return $ Just mix
                    else return Nothing
                    --tixExists <- doesFileExist (tmpDir ++ "/tmp.tix")
                    --if tixExists
                    --then readTix (tmpDir ++ "/tmp.tix")
                    --else return Nothing
            else return Nothing
    else return Nothing
