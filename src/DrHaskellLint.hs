import           Data.List
import           StaticAnalysis.CheckState
import           StaticAnalysis.Messages.ErrorToLint
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do
    args          <- getArgs
    (level, file, format) <- parse args
    run level file format

run level file format =
    case (level, file) of
        (1, file) -> (runChecksL1 file) >>= (putStrLn . lintErrors format)
        (_, _)    -> exitFailure

parse argv = do
    let x = case hasLevelHint argv of
                Just v  -> v
                Nothing -> 1
    let format = case "--json" ` elem ` argv of
                True  -> json
                False -> plain
    let y = argv !! 1
    return (x,y,format)

hasLevelHint [] = Nothing
hasLevelHint (('-':'-':'h':'i':'n':'t':'=':'l':level):_) =
    Just (read level :: Integer)
hasLevelHint (_:xs) = hasLevelHint xs

