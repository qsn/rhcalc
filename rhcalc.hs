--module RHCalc where

import System.Exit
import System.Environment (getArgs)
import System.Console.Readline
import qualified Data.Map as Map
import Data.Maybe (isJust,fromJust)
import Control.Monad (when)

import Stack (dumpstack, CalcError, Context(..), ctxBase)
import Core  (calc, st_dft)

-- interactive mode, console, main loop
-- C-d and "exit" quit
calc_main :: Context -> Maybe CalcError -> IO ()
calc_main ctx err = do
  putStr $ dumpstack (ctxBase ctx) (ctxStack ctx)
  printError err
  maybeLine <- readline "% "
  ((ctx', err'), c) <- case maybeLine of
    Nothing     -> return ((ctx, Nothing), False)
    Just "exit" -> return ((ctx, Nothing) ,False)
    Just args   -> do
      addHistory args
      return (unwrapError $ calc args ctx, True)
  if c then calc_main ctx' err' else return ()

unwrapError :: (Either CalcError a, Context) -> (Context, Maybe CalcError)
unwrapError (Left e,  ctx_error)  = (ctx_error, Just e)
unwrapError (Right _, ctx_success)= (ctx_success, Nothing)

-- display an error in console interactive mode
printError :: Maybe CalcError -> IO ()
printError err = when (isJust err) . putStrLn $ "  > " ++ show (fromJust err)

-- script/single input mode, evaluate one expression and exit
printResult :: (Either CalcError a, Context) -> IO a
printResult (Left err, _) = do
  putStrLn $ show err
  exitFailure
printResult (Right _, ctx) = do
  putStr $ dumpstack (ctxBase ctx) (ctxStack ctx)
  exitSuccess

-- standard mode: console, interactive
-- command-line arguments
--   -  read from standard input, evaluate, and exit
--   -e evaluate the string passed and exit
main :: IO ()
main = do
  args <- getArgs
  if "-" `elem` args
  then do
    expr <- getContents
    printResult $ calc expr st_dft
  else if length args > 1 && (args !! 0) == "-e"
       then do
         let expr = args !! 1
         printResult $ calc expr st_dft
       else calc_main st_dft Nothing
