--module RHCalc where

import System.Exit
import System.Environment (getArgs)
import System.Console.Readline
import qualified Data.Map as Map
import Data.Maybe (isJust,fromJust)
import Control.Monad (when)
import Control.Exception as X

import Stack (dumpstack, CalcError, Context(..), ctxBase)
import Core  (calc, st_dft)

prompt = "% "
errorPrefix = "  > "

do_calc_main :: Context -> IO ()
do_calc_main ctx = do
  maybeLine <- readline prompt
  ((err, ctx'), c) <- case maybeLine >>= exit of
    Nothing     -> return ((Nothing, ctx), False)
    Just args   -> do
      addHistory args
      return (calc args ctx, True)
  if c then calc_main (ctx, ctx') err else return ()
  where exit s = if s == "exit" then Nothing else Just s

-- interactive mode, console, main loop
-- C-d and "exit" quit
calc_main :: (Context,Context) -> Maybe CalcError -> IO ()
calc_main (safeCtx, ctx) err = do
  let s = dumpstack (ctxBase ctx) (ctxStack ctx)
  let s' = dumpstack (ctxBase safeCtx) (ctxStack safeCtx)
  newctx <- X.catch (success s ctx) (handler s' safeCtx)
  printError err
  do_calc_main newctx
  where success :: String -> a -> IO a
        success s res = do
          putStr s
          return res
        handler s ret e = do
          putStr s
          printErr e
          return ret
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
       else calc_main (st_dft,st_dft) Nothing
