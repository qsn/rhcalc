--module RHCalc where

import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import System.Console.Haskeline (runInputT, InputT, getInputLine, defaultSettings, outputStrLn, getHistory, putHistory)
import System.Console.Haskeline.History (historyLines, addHistory, emptyHistory)
import Control.Monad.Trans (liftIO)
import qualified Control.Exception as X
import System.Environment.XDG.BaseDir
import System.Directory (doesFileExist, createDirectoryIfMissing)

import Data.Maybe (isJust)
import Control.Monad (when)

import Data.Text (unpack)
import qualified Data.Text.IO as TIO

import Stack (dumpcontext, CalcError(OperationNotSupported), Context)
import Core  (calc, st_dft)

prompt, errorPrefix, xdgName :: String
prompt = "% "
errorPrefix = "  > "
xdgName = "rhcalc"

-- interactive mode, console, main loop
-- C-d and "exit" quit
repl :: Context -> InputT IO [String]
repl ctx = do
  maybeLine <- getInputLine prompt
  case maybeLine >>= exit of
    Nothing     -> fmap historyLines getHistory
    Just args   -> do
      let (merr, tryCtx) = calc args ctx
      when (isJust merr) $ printErrorM merr
      res <- liftIO $ X.tryJust isPatternMatchFail (try tryCtx)
      newCtx <- either handleErr return res
      repl newCtx
  where exit s = if s == "exit" then Nothing else Just s
        try ctx = do
          putStr $ dumpcontext ctx
          return ctx
        isPatternMatchFail (X.PatternMatchFail _) = Just OperationNotSupported
        handleErr err = printError err >> liftIO (try ctx) >> return ctx

-- display an error in console interactive mode
printError :: CalcError -> InputT IO ()
printError err = outputStrLn $ errorPrefix ++ show err
printErrorM :: Maybe CalcError -> InputT IO ()
printErrorM = mapM_ printError

-- script/single input mode, evaluate one expression and exit
printResult :: (Maybe CalcError, Context) -> IO a
printResult (Just err, _) = do
  putStrLn $ show err
  exitFailure
printResult (Nothing, ctx) = do
  putStr $ dumpcontext ctx
  exitSuccess

-- standard mode: console, interactive
-- command-line arguments
--   -  read from standard input, evaluate, and exit
--   -e evaluate the string passed and exit
main :: IO ()
main = do
  input <- getInput
  case input of
    Nothing -> do
      origHist <- readHistory
      hist <- runInputT defaultSettings $ replInit origHist
      writeHistory $ take (length hist - length origHist) hist
    Just expr -> printResult $ calc expr st_dft

getInput :: IO (Maybe String)
getInput = do
  args <- getArgs
  if "-" `elem` args
    then fmap Just getContents
    else if length args > 1 && (args !! 0) == "-e"
         then return $ Just (args !! 1)
         else return Nothing

readHistory :: IO [String]
readHistory = do
  hfp <- getUserDataFile xdgName "history"
  exists <- doesFileExist hfp
  if not exists then return []
    else do
      ls <- fmap unpack $ TIO.readFile hfp
      return $ reverse $ lines ls

writeHistory :: [String] -> IO ()
writeHistory hs = do
  hfp <- getUserDataFile xdgName "history"
  hdir <- getUserDataDir xdgName
  createDirectoryIfMissing True hdir
  appendFile hfp $ unlines $ reverse hs

replInit :: [String] -> InputT IO [String]
replInit origHist = do
  putHistory $ foldr addHistory emptyHistory origHist
  repl st_dft
