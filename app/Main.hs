--module RHCalc where

import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import System.Console.Readline (addHistory, readline)
import qualified Control.Exception as X

import Data.Maybe (isJust)
import Control.Monad (when)

import Stack (dumpcontext, CalcError(OperationNotSupported), Context)
import Core  (calc, st_dft)

prompt, errorPrefix :: String
prompt = "% "
errorPrefix = "  > "

-- interactive mode, console, main loop
-- C-d and "exit" quit
repl :: Context -> IO ()
repl ctx = do
  maybeLine <- readline prompt
  case maybeLine >>= exit of
    Nothing     -> return ()
    Just args   -> do
      addHistory args
      let (merr, tryCtx) = calc args ctx
      when (isJust merr) $ printErrorM merr
      res <- X.tryJust isPatternMatchFail (try tryCtx)
      newCtx <- either handleErr return res
      repl newCtx
  where exit s = if s == "exit" then Nothing else Just s
        try ctx = do
          putStr $ dumpcontext ctx
          return ctx
        isPatternMatchFail (X.PatternMatchFail _) = Just OperationNotSupported
        handleErr err = printError err >> try ctx >> return ctx

-- display an error in console interactive mode
printError :: CalcError -> IO ()
printError err = putStrLn $ errorPrefix ++ show err
printErrorM :: Maybe CalcError -> IO ()
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
    Nothing -> repl st_dft
    Just expr -> printResult $ calc expr st_dft

getInput :: IO (Maybe String)
getInput = do
  args <- getArgs
  if "-" `elem` args
    then fmap Just getContents
    else if length args > 1 && (args !! 0) == "-e"
         then return $ Just (args !! 1)
         else return Nothing
