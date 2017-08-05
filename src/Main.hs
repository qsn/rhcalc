--module RHCalc where

import System.Exit
import System.Environment (getArgs)
import System.Console.Readline
import qualified Data.Map as Map
import Data.Maybe (isJust,fromJust)
import Control.Monad (when)
import Control.Exception as X

import Stack (dumpcontext, CalcError, Context(..), ctxBase)
import Core  (calc, st_dft)

prompt = "% "
errorPrefix = "  > "

do_calc_main :: Context -> IO ()
do_calc_main ctx = do
  maybeLine <- readline prompt
  case maybeLine >>= exit of
    Nothing     -> return ()
    Just args   -> do
      addHistory args
      let (err, ctx') = calc args ctx
      calc_main (ctx, ctx') err
  where exit s = if s == "exit" then Nothing else Just s

-- interactive mode, console, main loop
-- C-d and "exit" quit
calc_main :: (Context,Context) -> Maybe CalcError -> IO ()
calc_main (safeCtx, ctx) err = do
  newctx <- X.catch (try ctx) (handler safeCtx)
  printError err
  do_calc_main newctx
  where try ctx = do
          putStr $ dumpcontext ctx
          return ctx
        handler ctx e = do
          putStr $ dumpcontext ctx
          printErr e
          return ctx
        printErr :: SomeException -> IO ()
        printErr e =  do
          case (fromException e) :: Maybe PatternMatchFail of
           Just x -> putStrLn "  > operation not supported"
           nothing -> return ()

-- display an error in console interactive mode
printError :: Maybe CalcError -> IO ()
printError err = when (isJust err) . putStrLn $ errorPrefix ++ show (fromJust err)

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
