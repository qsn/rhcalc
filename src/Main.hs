--module RHCalc where

import System.Exit
import System.Environment (getArgs)
import System.Console.Readline
import qualified Control.Exception as X

import Stack (dumpcontext, CalcError(OperationNotSupported), Context)
import Core  (calc, st_dft)

prompt = "% "
errorPrefix = "  > "

-- interactive mode, console, main loop
-- C-d and "exit" quit
calc_main :: (Context,Context) -> Maybe CalcError -> IO ()
calc_main (safeCtx, ctx) err = do
  res <- X.tryJust isPatternMatchFail (try ctx)
  newSafeCtx <- case res of
    Left err -> printError err >> return safeCtx
    Right ctx -> printError' err >> return ctx
  maybeLine <- readline prompt
  case maybeLine >>= exit of
    Nothing     -> return ()
    Just args   -> do
      addHistory args
      let (err, newCtx) = calc args newSafeCtx
      calc_main (newSafeCtx, newCtx) err
  where try ctx = do
          putStr $ dumpcontext ctx
          return ctx
        exit s = if s == "exit" then Nothing else Just s
        isPatternMatchFail (X.PatternMatchFail _) = Just OperationNotSupported

-- display an error in console interactive mode
printError err = putStrLn $ errorPrefix ++ show err
printError' = sequence_ . fmap printError

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
    Nothing -> calc_main (st_dft,st_dft) Nothing
    Just expr -> printResult $ calc expr st_dft

getInput :: IO (Maybe String)
getInput = do
  args <- getArgs
  if "-" `elem` args
    then fmap Just getContents
    else if length args > 1 && (args !! 0) == "-e"
         then return $ Just (args !! 1)
         else return Nothing
