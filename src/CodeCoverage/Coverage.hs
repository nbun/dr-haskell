{-# LANGUAGE ScopedTypeVariables #-}
module CodeCoverage.Coverage (module CodeCoverage.Coverage) where

import           Control.Exception
import           Language.Haskell.Exts
import           Paths_drhaskell
import           StaticAnalysis.Messages.ErrorToLint
import           StaticAnalysis.Messages.Prettify
import           System.Directory
import           System.IO
import           System.IO.Temp
import           System.Process
import           Trace.Hpc.Mix
import           Trace.Hpc.Tix
import           Trace.Hpc.Util
import           Util.ModifyAst

getConverageOutput :: ModifiedModule -> IO [Lint]
getConverageOutput m = do
    let plainFile = printModified m
    mixtix <- (withSystemTempDirectory "drhaskell-lint."
                    $ runInTempDir plainFile)
    case mixtix of
        Just (mix, tix) -> return $ parseMT mix tix
        Nothing         -> return []

--Mix FilePath UTCTime Hash Int [MixEntry]
parseMT :: Mix -> Tix -> [Lint]
parseMT mix@(Mix _ _ hash1 _ mixpos) (Tix (TixModule _ hash2 _ tixpos:xs))
    | hash1 == hash2 =
        let zipped = zip tixpos mixpos
            filtered = filter (\(i,_) -> i /= 0) zipped
            filteredMixPos = foldr (\(_,x) xs -> x : xs) [] filtered
        in loopMixPos filteredMixPos
    | otherwise = parseMT mix (Tix xs)
        where loopMixPos :: [MixEntry] -> [Lint]
              loopMixPos []            = []
              loopMixPos ((pos,_):mes) = Lint "HPC" (restorePosition $ fromHpcPos pos) Warning "Covered" : loopMixPos mes -- data Lint = Lint Filename Position MessageClass Message
              restorePosition :: Position -> Position
              restorePosition (sl, sc, el, ec) = (sl - 1, sc, el - 1, ec)
--parseMT mix (Tix (x:xs)) = parseMT mix (Tix xs)

extractStartAndEndColumn :: [Integer] -> (Int, Int)
extractStartAndEndColumn [] = (0,0)
extractStartAndEndColumn xs = (fromIntegral $ head xs,
                               fromIntegral $ last xs)

runInTempDir :: String -> FilePath -> IO (Maybe (Mix,Tix))
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
                    tixExists <- doesFileExist (tmpDir ++ "/tmp.tix")
                    if hpcDirExists && tixExists
                    then do mix <- readMix [(tmpDir ++ "/.hpc")] (Left "Main")
                            tix <- readTix (tmpDir ++ "/tmp.tix")
                            case tix of
                                Just t  -> return $ Just (mix, t)
                                Nothing -> return Nothing
                    else return Nothing
            else return Nothing
    else return Nothing
