{-# LANGUAGE TupleSections #-}

module Repl.Main (module Repl.Main) where

import           Control.Lens                 hiding (Level)
import           Control.Monad.Catch          as MC
import           Control.Monad.State
import           Paths_drhaskell
import           Data.Version (showVersion)
import           System.FilePath
import           Data.List

import           Language.Haskell.Interpreter
import           Repl.CmdOptions
import           Repl.Loader
import           Repl.Types
import           System.Console.Haskeline

{-
Current Limitations:
  - history is not used
  - no let-constructs
-}


replRead :: Repl (Maybe String)
replRead = do
  mods <- liftInterpreter getLoadedModules
  let filtered = mods \\ ["MyPrelude", "Tests", "StartupEnvironment"]
      prompt = if   null filtered
               then "Dr. Haskell"
               else intercalate ", " filtered
  level <- use currentLevel
  liftInput $ getInputLine $ prompt ++ " ("++ printLevel level ++")> "

replPrint :: Maybe String -> ReplInput ()
replPrint Nothing  = return ()
replPrint (Just x) = outputStrLn x

replLoop :: Repl ()
replLoop = do
  minput <- liftRepl replRead
  case minput of
       Nothing -> return ()
       Just x -> do
         (res, cont) <- replEval x
         liftInput $ replPrint res
         when cont replLoop

initInterpreter :: Repl ()
initInterpreter = do
  datadir <- liftIO getDataDir
  liftInterpreter $ Language.Haskell.Interpreter.set
    [searchPath := [".", datadir </> "TargetModules"]]
  loadInitialModules

main :: IO ()
main = do
  initialState <- handleCmdArgs
  res <- runRepl initialState $ do
    initInterpreter
    fname <- use filename
    unless (null fname) $ do
      errors <- liftRepl $ loadModule fname
      liftInput $ replPrint (Just (unlines $ map printLoadMessage errors))
    liftInput showBanner
    replLoop
  case res of
       Left err -> putStrLn $ "Error:" ++ show err
       Right _  -> return ()

replEval :: String -> Repl (Maybe String, Bool)
replEval q = case q of
  ':':xs -> replEvalCommand xs
  _      -> liftInterpreter $ (,True) <$> replEvalExp q

replHelp :: Maybe String -> Repl String
replHelp input = return $ init $ unlines $ hint [
  "Usage:",
  ":? - This help",
  ":l - load module",
  ":r - reload module",
  ":t - evaluate type",
  "expression - evaluate expression" ]
  where hint xs = case input of
                    Just  s -> ("Unrecognized option '" ++ s ++ "'") : xs
                    Nothing -> xs

replEvalExp :: String -> ReplInterpreter (Maybe String)
replEvalExp q =
  MC.handleAll (\e -> do
                      liftIO $ putStrLn (displayException e)
                      return Nothing) $ do
    t <- typeOf q
    if t == "IO ()"
      then do
        --apparently the interpreter does not put the output through 'our'
        --stdout, so we need to flush the buffer *within* the interpreter,
        --which makes this really messy. Seems to work though.
        action <- interpret
                    ("("++q++")>>System.IO.hFlush System.IO.stdout")
                    (as :: IO ())
        liftIO action
        return Nothing
      else Just <$> eval q

replEvalCommand :: String -> Repl (Maybe String, Bool)
replEvalCommand cmd = if null cmd then invalid cmd else
  case head args of
    "q"      -> quit
    "quit"   -> quit
    "l"      -> load
    "load"   -> load
    "r"      -> reload
    "reload" -> reload
    "t"      -> typeof
    "type"   -> typeof
    "?"      -> help
    "h"      -> help
    "help"   -> help
    s        -> invalid s
  where args = words cmd
        quit = return (Nothing, False)
        load = let fn = args !! 1 in do
          liftRepl $ modify (Control.Lens.set filename fn)
          errors <- loadModule fn
          return $ (,) (Just (unlines $ map printLoadMessage errors)) True
        reload = do
          md <- gets _filename
          if (null md)
          then
            return (Just "Ok, modules loaded: none.", True)
          else do
            errors <- loadModule md
            return $ (,) (Just (unlines $ map printLoadMessage errors)) True
        typeof =  liftInterpreter (typeOf $ args !! 1) >>=
                                  \res -> return (Just res, True)
        invalid s =  replHelp (Just s) >>= \res -> return (Just res, True)
        help = (,True) <$> Just <$> replHelp Nothing

--TODO: some better ascii art?
showBanner :: ReplInput ()
showBanner = outputStrLn $ unlines [
  "",
  "\\ \\      Dr. Haskell version " ++ showVersion version,
  " \\ \\",
  " /  \\    Type \":?\" for help.",
  "/ /\\ \\"]