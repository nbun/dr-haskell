module Repl.Main (module Repl.Main) where

import           Control.Lens                     hiding (Level, set)
import           Control.Monad.Catch              as MC
import           Control.Monad.State
import           Paths_drhaskell
import           System.FilePath

import           Language.Haskell.Interpreter
import           Repl.CmdOptions
import           Repl.Loader
import           Repl.Types
import           System.Console.Haskeline

import           StaticAnalysis.Messages.Prettify

{-

Current Limitations:
  - history is not used
  - no let-constructs
  - our custom checks and modifications
  - anything, really
-}



replRead :: ReplInput (Maybe String)
replRead = getInputLine "Dr. Haskell> "

replPrint :: Maybe String -> ReplInput ()
replPrint Nothing  = return ()
replPrint (Just x) = outputStrLn x

replLoop :: Repl ()
replLoop = do
  minput <- liftInput replRead
  case minput of
       Nothing -> return ()
       Just x -> do
         (res, cont) <- replEval x
         liftInput $ replPrint res
         when cont replLoop

initInterpreter :: ReplInterpreter ()
initInterpreter = do
  datadir <- liftIO getDataDir
  set [searchPath := [".", datadir </> "TargetModules"]]
  setImports ["Prelude"]

main :: IO ()
main = do
  initialState <- handleCmdArgs
  res <- runRepl initialState $ do
    liftInterpreter initInterpreter
    fname <- use filename
    unless (null fname) $ do
      errors <- liftRepl $ loadModule fname
      liftInput $ replPrint (Just (unlines $ map printLoadMessage errors))
    replLoop
  case res of
       Left err -> putStrLn $ "Error:" ++ show err
       Right _  -> return ()

replEval :: String -> Repl (Maybe String, Bool)
replEval q = case q of
  ':':xs -> replEvalCommand xs
  _      -> liftInterpreter $ replEvalExp q >>= \res -> return (res, True)

replHelp :: Maybe String -> Repl String
replHelp input = return $ unlines $ hint : [
  ":? - This help",
  ":l - load module",
  ":r - reload module",
  ":t - evaluate type",
  "expression - evaluate expression" ]
  where hint = case input of
                 Just  s -> "Unrecognized option '" ++ s ++ "'"
                 Nothing -> ""

replEvalExp :: String -> ReplInterpreter (Maybe String)
replEvalExp q =
  MC.handleAll (\e -> do
                      liftIO $ putStrLn (displayException e)
                      return Nothing) $ do
    t <- typeOf q
    if t == "IO ()"
      then interpret q (as :: IO ()) >>= liftIO >> return Nothing
      else Just <$> eval q

replEvalCommand :: String -> Repl (Maybe String, Bool)
replEvalCommand cmd = case cmd of
  "q" -> return (Nothing, False)
  ('l':' ': xs)-> do
    previousForceLevel <- use forceLevel
    liftRepl $ forceLevel .= previousForceLevel
    errors <- loadModule $ head (words xs)
    return $ (,) (Just (unlines $ map printLoadMessage errors)) True
  ('r':_) -> do
    md <- gets _filename
    errors <- loadModule md
    return $ (,) (Just (unlines $ map printLoadMessage errors)) True
  ('t':' ': xs) -> liftInterpreter (typeOf xs) >>= \res -> return (Just res, True)
  s -> (replHelp $ Just s) >>= \res -> return (Just res, True)
