-- | The module DrHaskellLint provides the whole "call of the linter"
--   -functionallity.
-- Additionally it provides the call of the tests.
module DrHaskellLint (module DrHaskellLint) where

import           CodeCoverage.Coverage
import           Control.Lens                         hiding (Level)
import           Data.List
import           Data.List.Utils
import           Data.Maybe
import           Language.Haskell.Exts
import qualified Language.Haskell.HLint3              as Hlint
import           Repl.CmdOptions
import           Repl.Loader
import           Repl.Types
import           StaticAnalysis.CheckState
import           StaticAnalysis.Level
import           StaticAnalysis.Messages.ErrorToLint
import           StaticAnalysis.Messages.Prettify
import           StaticAnalysis.Messages.StaticErrors
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           TypeInference.AbstractHaskell
import           TypeInference.Main
import           Util.ModifyAst

-- | Entry point for the cli call
main :: IO ()
main = do
    args <- getArgs
    if length args < 2 -- check if we have enough parameters to run
        then exitFailure
        else do
            (level, file, format) <- parseArgs args
            -- parse the cli parameters for level,
            -- filename and the output format (plain,json)
            file' <- manipulatePathWithHostvar file --for dockerised running
            run level file' format -- the running, testing and linting

manipulatePathWithHostvar :: String -> IO String
manipulatePathWithHostvar file = do
    hostpath <- lookupEnv "DRHASKELLHOSTPATH"
    case hostpath of
        Nothing   -> return file
        Just path -> return $ replace path "/tmp/drhaskell-src" file

-- | Determines from the level the correct levelCode and invokes hlint
--   (including conversion of linting datastructures).
run :: Integer -> String -> LinterOutput -> IO ()
run level file format = do
    hlintIdeas <- pushToHlint file -- run hlint
    let hlintHints = hLintToLint file hlintIdeas -- convert hlint to lint
    let lvl = case level of -- determine levelcode
                    1 -> Level1
                    2 -> Level2
                    3 -> Level3
                    _ -> LevelFull
    runWithRepl hlintHints file lvl format -- run the rest of the linting

-- | Calls the analysis used in our Repl and attaches coverage and hlint results
runWithRepl :: [Lint] -> String -> Level -> LinterOutput -> IO ()
runWithRepl hlintHints file lvl format = do
    let state = forceLevel .~ Just lvl $ initialLintReplState -- get Repl state
    parseRes <- parseModified file -- Parse input file with repl impl
    case parseRes of
      ParseOk m1 -> do
        (m2, errs) <- transformModule [] state m1 -- "
        errs' <- runCheckLevel lvl file -- run checks
        tires <- inferModule (modifiedModule m1)
        let tiErrors =
              case (useOwnTI lvl, tires) of -- run type inference
                (True, Left e)  -> let pos = posOfTIError e
                                    in [TypeError pos e]
                (_,         _) -> []
        coverage <- getConverageOutput m2 -- run coverage
        putStrLn (lintErrorHlint (hlintHints ++ coverage) format (Just lvl)
                   (errs ++ errs' ++ if null errs' then tiErrors else []))
        -- build output
      ParseFailed pos m ->
        putStrLn $ lintErrorHlint [buildParseError pos m] format (Just lvl) []

-- | Invokes hlint via hlint module
pushToHlint :: String -> IO [Hlint.Idea]
pushToHlint file = Hlint.hlint [file, "--quiet"]

-- | Converts Hlint Ideas to Lints
hLintToLint :: String -> [Hlint.Idea] -> [Lint]
hLintToLint _ [] = []
hLintToLint file (x:xs) =
    Lint file
         (extractStartPosition (Hlint.ideaSpan x))
         -- get startposition from hlint idea spaninfo
         (severityToMessageClass (Hlint.ideaSeverity x))
         -- get message class from hlint idea severity
         (Hlint.ideaHint x)
    : hLintToLint file xs

-- | Convert Severity into MessagaeClass
severityToMessageClass :: Hlint.Severity -> MessageClass
severityToMessageClass Hlint.Suggestion = Suggestion
severityToMessageClass Hlint.Warning    = Warning
severityToMessageClass Hlint.Error      = Error
severityToMessageClass Hlint.Ignore     = Suggestion

-- | Naive implementation of parameter parsing
-- simple occurence check and some pattern matching for file extraction
parseArgs :: [String] -> IO (Integer, String, LinterOutput)
parseArgs argv = do
    let x = fromMaybe 0 (hasLevelHint argv) -- get level
    let format = if "--json" `elem` argv -- json or plain
                 then json
                 else plain {- --plain -}
    let y = findFile argv -- get file
    return (x, y, format)

-- | Simple implementation to get the filename
findFile :: [String] -> String
findFile []               = ""
findFile (('-':'-':_):xs) = findFile xs
findFile (x:_)            = x

-- | Get leve from hint-Parameter (since most of the default linter
--   implementations provide the hint-Parameter in there config gui)
hasLevelHint :: [String] -> Maybe Integer
hasLevelHint [] = Nothing
hasLevelHint (('-':'-':'h':'i':'n':'t':'=':'l':level):_) =
    Just (read level :: Integer)
hasLevelHint (_:xs) = hasLevelHint xs
