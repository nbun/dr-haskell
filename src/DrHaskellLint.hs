module DrHaskellLint (module DrHaskellLint) where

import           Control.Lens                        hiding (Level)
import           Data.List
import           Data.Maybe
import           Language.Haskell.Exts
import qualified Language.Haskell.HLint3             as Hlint
import           Repl.CmdOptions
import           Repl.Loader
import           Repl.Types
import           StaticAnalysis.CheckState
import           StaticAnalysis.Messages.ErrorToLint
import           StaticAnalysis.Messages.Prettify
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Util.ModifyAst

main :: IO ()
main = do
    args <- getArgs
    if length args /= 3
        then exitFailure
        else do
            (level, file, format) <- parseArgs args
            run level file format

run :: Integer -> String -> LinterOutput -> IO ()
run level file format = do
    hlintIdeas <- pushToHlint file
    hlintHints <- return (hLintToLint file hlintIdeas)
    let lvl = case level of
                    1 -> Level1
                    2 -> Level2
                    3 -> Level3
                    _ -> LevelFull
    runWithRepl hlintHints file lvl format

runWithRepl :: [Lint] -> String -> Level -> LinterOutput -> IO ()
runWithRepl hlintHints file lvl format = do
    let state = forceLevel .~ Just lvl $ initialReplState
    ParseOk m1 <- parseModified file
    (m2, errs) <- transformModule [] state m1
    errs' <- runCheckLevel lvl file
    putStrLn (lintErrorHlint hlintHints format (errs ++ errs'))

pushToHlint :: String -> IO [Hlint.Idea]
pushToHlint file = Hlint.hlint [file, "--quiet"]

hLintToLint :: String -> [Hlint.Idea] -> [Lint]
hLintToLint _ [] = []
hLintToLint file (x:xs) =
    Lint file (extractStartPosition (Hlint.ideaSpan x)) (severityToMessageClass (Hlint.ideaSeverity x)) (Hlint.ideaHint x)
    : hLintToLint file xs

severityToMessageClass :: Hlint.Severity -> MessageClass
severityToMessageClass Hlint.Suggestion = Suggestion
severityToMessageClass Hlint.Warning    = Warning
severityToMessageClass Hlint.Error      = Error
severityToMessageClass Hlint.Ignore     = Suggestion

parseArgs :: [String] -> IO (Integer, String, LinterOutput)
parseArgs argv = do
    let x = fromMaybe 0 (hasLevelHint argv)
    let format = if "--json" ` elem ` argv
                 then json
                 else plain
    let y = argv !! 1
    return (x, y, format)

hasLevelHint :: [String] -> Maybe Integer
hasLevelHint [] = Nothing
hasLevelHint (('-':'-':'h':'i':'n':'t':'=':'l':level):_) =
    Just (read level :: Integer)
hasLevelHint (_:xs) = hasLevelHint xs
