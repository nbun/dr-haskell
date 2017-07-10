-- | Provides the coverage analysis by compiling and running the given file
{-# LANGUAGE ScopedTypeVariables #-}
module CodeCoverage.Coverage (
    getConverageOutput
) where

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

-- | Main interface for running the coverage analysis
getConverageOutput :: ModifiedModule -> IO [Lint]
getConverageOutput m = do
    let plainFile = printModified m -- get plain filecontent
    -- compile, run and check inside a temp dir
    mixtix <- withSystemTempDirectory "drhaskell-lint."
                    $ runInTempDir plainFile
    case mixtix of
        Just (mix, tix) -> return $ parseMT mix tix -- parse the check output
        Nothing         -> return []

-- | Takes the Mix and the Tix information provided by the hpc impl
--   and converts it into coverage information
--   readable by the linter
-- Mix FilePath UTCTime Hash Int [MixEntry]
parseMT :: Mix -> Tix -> [Lint]
parseMT mix@(Mix _ _ hash1 _ mixpos) (Tix (TixModule _ hash2 _ tixpos:xs))
    | hash1 == hash2 = -- get the right mix tix combination
        let zipped = zip tixpos mixpos
            filtered = filter (\(i,_) -> i /= 0) zipped
            -- filter only the lines hit
            filteredMixPos = foldr (\(_,x) xs -> x : xs) [] filtered
        in loopMixPos filteredMixPos -- lint them
    | otherwise = parseMT mix (Tix xs) -- next tix if hashed dont match
        where loopMixPos :: [MixEntry] -> [Lint] -- build the lintoutput
              loopMixPos []            = []
              loopMixPos ((pos,_):mes) = Lint "HPC"
                                              (restorePosition $ fromHpcPos pos)
                                              Warning "Covered"
                                         : loopMixPos mes
              -- data Lint = Lint Filename Position MessageClass Message
              restorePosition :: Position -> Position
              restorePosition (sl, sc, el, ec) = (sl - 1, sc, el - 1, ec)
              -- some position adjustment. maybe this needs some adjustments
              -- if there are more includes at the start of the file

-- | Get start and end position from tickarray
extractStartAndEndColumn :: [Integer] -> (Int, Int)
extractStartAndEndColumn [] = (0,0)
extractStartAndEndColumn xs = (fromIntegral $ head xs,
                               fromIntegral $ last xs)

-- | Implementation of temp dir compiling, running and checking
runInTempDir :: String -> FilePath -> IO (Maybe (Mix,Tix))
runInTempDir plainFile tmpDir = do
    setCurrentDirectory tmpDir -- move to tempdir
    -- get datadir from cabal config for include search inside of ghc
    datadir <- getDataDir
    -- write the actual file which we want to analyse
    writeFile (tmpDir ++ "/tmp.hs") plainFile
    tmpExists <- doesFileExist (tmpDir ++ "/tmp.hs")
    if tmpExists
    then do (_, _, _) <- readProcessWithExitCode
                            "ghc"
                            ["-fhpc",
                             "tmp.hs",
                             "-o",
                             "tmp",
                             "-idirs:" ++ datadir ++ "/TargetModules"]
                            []
            -- run ghc with hpc flag and append include dir from cabal datadir
            tmpCompExists <- doesFileExist (tmpDir ++ "/tmp")
            if tmpCompExists
            then do (_, _, _) <- readProcessWithExitCode "./tmp" [] []
                    -- run the compiled file to generate coverage output
                    hpcDirExists <- doesDirectoryExist (tmpDir ++ "/.hpc")
                    tixExists <- doesFileExist (tmpDir ++ "/tmp.tix")
                    if hpcDirExists && tixExists
                    -- are all files we need present
                    then do mix <- readMix [tmpDir ++ "/.hpc"] (Left "Main")
                            tix <- readTix (tmpDir ++ "/tmp.tix")
                            case tix of
                                Just t  -> return $ Just (mix, t)
                                -- return mix and tix
                                Nothing -> return Nothing
                    else return Nothing
            else return Nothing
    else return Nothing
