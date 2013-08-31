import System.Console.Readline
import qualified Data.Map as Map

import Stack (dumpstack)
import Core  (calc)

calc_main ys vars = do
  putStr $ dumpstack ys
  maybeLine <- readline "% "
  ((zs,vs),c) <- case maybeLine of
    Nothing     -> return (([],Map.empty),False)
    Just "exit" -> return (([],Map.empty),False)
    Just args   -> do addHistory args ; return (calc args (ys,vars), True)
  if c then calc_main zs vs else return ()

main = calc_main [] (Map.empty)
