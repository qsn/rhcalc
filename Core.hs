module Core
  (
    calc
  , fct_run
  , parse
  , rmquotes
  )
  where

import Data.List
import qualified Data.Map as Map
import Control.Applicative

import {-# SOURCE #-} Operators
import Stack

type Memory = Map.Map String Symbol
type CoreFct = [Symbol] -> Memory -> ([Symbol],Memory)

functions :: Map.Map String (Int, Int, CoreFct)
functions = Map.fromList [("store",(2,0,fct_store)),
                          ("clear",(1,0,fct_clear)),
                          ("clearall",(0,0,fct_clearall)),
                          ("vars",(0,1,fct_showvars)),
                          ("run",(-2,-1,fct_run))]

findfct :: String -> Maybe (Int,Int,CoreFct)
findfct x = Map.lookup x functions

fct_store :: CoreFct
fct_store ss@[String name,var] vars = let n = rmquotes name
                                   in case (findfct n, findoperator n, findvar n vars) of
                                     (Just _,    _,      _ )     -> (ss,vars) -- found a function or operator with this name? cancel
                                     (Nothing, Just _,   _ )     -> (ss,vars)
                                     (Nothing, Nothing, Just _)  -> ([], Map.update (\_ -> Just var) n vars)
                                     (Nothing, Nothing, Nothing) -> ([], Map.insert n var vars)

fct_clear :: CoreFct
fct_clear [String name]     vars = let name' = filter (/='"') name in ([], Map.delete name' vars)
fct_clearall :: CoreFct
fct_clearall _ _ = ([],Map.empty)
fct_showvars :: CoreFct
fct_showvars _ vars = ([String $ show $ Map.toList vars],vars)
fct_run :: CoreFct
fct_run ((String script):ys) vars = calc (rmquotes script) (ys,vars)


findvar :: String -> Memory -> Maybe Symbol
findvar name = Map.lookup name

stack :: Maybe [String] -> (Stack,Memory) -> (Stack,Memory)
stack  Nothing        ss    = ss
stack (Just [])       ss    = ss
stack (Just (x:xs)) (ys,vs) = case (findfct x, findoperator x, findvar x vs) of
  (Just fct,    _,      _ )       -> stack (Just xs) $ runfct fct (ys,vs)          -- found a function? use it
  (Nothing , Just op,   _ )       -> stack (Just xs) (     run op ys,          vs) -- found an operator? use it
  (Nothing , Nothing, Just v) -> stack (Just xs) (         v         : ys, vs)     -- found a variable? put it on the stack
  (Nothing , Nothing, Nothing)    -> stack (Just xs) ((read x :: Symbol) : ys, vs) -- no such variable or operator? stack

runfct :: (Int,Int,CoreFct) -> (Stack,Memory) -> (Stack,Memory)
runfct (argc,rc,f) (ys,vs)
  | length argv == n = let (xs,us) = f argv vs in (xs ++ zs, us)
  where (n,ys') = case argc of
          -1 -> (round.tonum $ head ys, tail ys)
          -2 -> (length ys, ys)
          _ -> (argc, ys)
        (argv,zs) = splitAt n ys'


calc :: String -> (Stack,Memory) -> (Stack,Memory)
calc args (ys,vars) = stack (parse args) (ys,vars)


parse :: String -> Maybe [String]
parse = glue [] (0,0)
  where glue [] (0,0) [] = Just [] -- reached the end of the input and all blocks are closed? good parse
        glue xs (0,0) [] = Just [reverse xs]
        glue xs (0,0) (' ':ys) = (:) <$> pure (reverse xs) <*> glue [] (0,0) ys
        glue xs (a,b) (y:ys)
          | a' < 0    = Nothing    -- a block has been closed that hadn't been opened before? no parse
          | otherwise = glue (y : xs) (a',b') ys
          where (a',b') = case y of
                  '(' -> (a+1, b)
                  ')' -> (a-1, b)
                  '"' -> (a  , 1-b)
                  _   -> (a  , b)
        glue _    _   [] = Nothing -- reached the end of the input without closing all blocks? no parse

-- removes first and final quote, if present
rmquotes [] = []
rmquotes ss@(c:cs) = let cs' = reverse . rmquotes . reverse $ cs in if c == '"' then cs' else ss
