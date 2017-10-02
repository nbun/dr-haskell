{-# LANGUAGE TupleSections #-}

module Repl.Main (module Repl.Main) where

import Control.Lens                         hiding (Level)
import Control.Monad.Catch                  as MC
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe                           (fromJust, isJust)
import Data.Version                         (showVersion)
import Language.Haskell.Exts.Parser
import Language.Haskell.Interpreter
import Paths_drhaskell
import Repl.CmdOptions
import Repl.Loader
import Repl.Types
import StaticAnalysis.CheckState
import System.Console.Haskeline
import System.FilePath
import TypeInference.AbstractHaskell        (defaultAHOptions, showTypeExpr, AHOptions(..))
import TypeInference.AbstractHaskellGoodies (exprType')
import TypeInference.Main

{-
Current Limitations:
  - history is not used
  - no let-constructs
-}


replRead :: Repl (Maybe String)
replRead = do
  level  <- use currentLevel
  prompt <- use promptModule
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
    liftInput showBanner
    initInterpreter
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
  _      -> (,True) <$> replEvalExp q

replHelp :: Maybe String -> Repl String
replHelp input = return $ init $ unlines $ hint [
  "Usage:",
  ":? - This help",
  ":l - load module",
  ":q - quit",
  ":r - reload module",
  ":t - evaluate type",
  "expression - evaluate expression" ]
  where hint xs = case input of
                    Just  s -> ("Unrecognized option '" ++ s ++ "'") : xs
                    Nothing -> xs

replEvalExp :: String -> Repl (Maybe String)
replEvalExp q = case filter (not . isSpace) q of
    "" -> return Nothing
    _  ->
      MC.handleAll (\e -> do
                          liftIO $ putStrLn (displayException e)
                          return Nothing) $ do
        errors <- checkType q
        if isJust errors
        then return errors
        else do
          t <- liftInterpreter $ typeOf q
          if t == "IO ()"
            then do
              --apparently the interpreter does not put the output through 'our'
              --stdout, so we need to flush the buffer *within* the interpreter,
              --which makes this really messy. Seems to work though.
              action <- liftInterpreter $ interpret
                          ("("++q++")>>System.IO.hFlush System.IO.stdout")
                          (as :: IO ())
              liftIO action
              return Nothing
            else liftInterpreter $ Just <$> eval q
  where
    checkType :: String -> Repl (Maybe String)
    checkType q = do
      p' <- use tiProg
      case p' of
           [] -> return Nothing
           ps -> case parseExpWithMode (defaultParseMode{parseFilename = "<interactive>"}) q of
                      ParseFailed _ f -> return $ Just f
                      ParseOk e       ->
                        case inferHSEExp ps e of
                             Left e -> return $ Just
                                              $ showTIError
                                                defaultAHOptions {
                                                  unqModules =
                                                    "Prelude" :
                                                    unqModules
                                                      defaultAHOptions
                                                }
                                                e
                             Right e -> return Nothing

replEvalCommand :: String -> Repl (Maybe String, Bool)
replEvalCommand cmd = if null cmd then invalid cmd else
  case head args of
    "q"      -> quit
    "quit"   -> quit
    "l"      -> load
    "load"   -> load
    "r"      -> reload
    "reload" -> reload
    "t"      -> commandTypeof args
    "type"   -> commandTypeof args
    "?"      -> help
    "h"      -> help
    "help"   -> help
    s        -> invalid s
  where args = words cmd
        quit = return (Nothing, False)
        load = case args of
          [_]       -> return (Just "No File specified", True)
          (_:_:_:_) -> return (Just "Cannot load multiple files.", True)
          [_,fn]    -> do
            liftRepl $ modify (Control.Lens.set filename fn)
            errors <- loadModule fn
            return $ (,) (Just (unlines $ map printLoadMessage errors)) True
        reload = do
          md <- gets _filename
          if null md
          then
            return (Just "Ok, modules loaded: none.", True)
          else do
            errors <- loadModule md
            return (Just $ unlines $ map printLoadMessage errors, True)
        invalid s =  replHelp (Just s) >>= \res -> return (Just res, True)
        help = (,True) . Just <$> replHelp Nothing

commandTypeof :: [String] -> Repl (Maybe String, Bool)
commandTypeof [_]  = return (Just "Expression expected", True)
commandTypeof args = do
    l <- use currentLevel
    case l of
         Level1    -> commandTypeofTI
         Level2    -> commandTypeofTI
         Level3    -> commandTypeofTI
         LevelFull -> commandTypeofHint
  where
    commandTypeofHint = MC.handleAll (\e ->
                          return (Just (displayException e), True)) $
                        liftInterpreter (typeOf expression) >>=
                        \res -> return (Just (expression ++
                                              " :: " ++
                                              fixType res), True)
    commandTypeofTI = do
      p' <- use tiProg
      case p' of
           [] -> return (Nothing, True)
           ps -> case parseExpWithMode (defaultParseMode{parseFilename = "<interactive>"}) expression of
                      ParseFailed _ f -> return (Just f, True)
                      ParseOk e       ->
                        case inferHSEExp ps e of
                             Left e -> return (Just $ showTIError
                                                      defaultAHOptions {
                                                        unqModules =
                                                          "Prelude" :
                                                          unqModules
                                                            defaultAHOptions
                                                      }
                                                      e, True)
                             Right e -> return (Just
                                                 (expression ++
                                                  " :: "     ++
                                                  (showTypeExpr
                                                    defaultAHOptions {
                                                      unqModules =
                                                        "Prelude" :
                                                        unqModules
                                                          defaultAHOptions
                                                    } $
                                                    fromJust         $
                                                    exprType' e)), True)
    expression = unwords $ tail args
    fixType "Prelude.Num a => a"         = "Int"
    fixType         "Num a => a"         = "Int"
    fixType "GHC.Num.Num a => a"         = "Int"
    fixType "GHC.Real.Fractional a => a" = "Float"
    fixType          "Fractional a => a" = "Float"
    fixType x                            = x

--TODO: some better ascii art?
showBanner :: ReplInput ()
showBanner = outputStrLn $ unlines [
  "",
  "\\ \\      Dr. Haskell version " ++ showVersion version,
  " \\ \\",
  " /  \\    Type \":?\" for help.",
  "/ /\\ \\"]
