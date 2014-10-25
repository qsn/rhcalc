module Core
  (
    calc
  , contextFromStack
  , st_dft
  , CoreFct
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
import Data.Maybe

--import {-# SOURCE #-} Operators
import Operators
import Stack

type CoreFct = ErrorT CalcError (State Context) ()


contextFromStack :: Stack -> Context
contextFromStack s = Context { ctxStack = s, ctxMemory = Map.empty, ctxSettings = defaultSettings }

st_dft = contextFromStack [] :: Context

--
-- basic functions
--
functions :: Map.Map String CoreFct
functions = Map.fromList [("store", fct_store),
                          ("clear", fct_clear),
                          ("cls", fct_clearstack),
                          ("clearall", fct_clearall),
                          ("vars", fct_showvars),
                          ("base", fct_base),
                          ("settings", fct_showsettings),
                          ("run", fct_run)]

findfct :: String -> Maybe CoreFct
findfct name = Map.lookup name functions

-- empty the stack
fct_clearstack :: CoreFct
fct_clearstack = modify $ \ctx -> ctx { ctxStack = [] }

-- clear all variable bindings
fct_clearall :: CoreFct
fct_clearall = modify $ \ctx -> ctx { ctxMemory = Map.empty }

-- clear a variable binding with the name at the top of the stack
fct_clear :: CoreFct
fct_clear = do
  name <- pop
  ifString name $ \n -> let name = rmquotes n
                        in modify $ \ctx -> ctx { ctxMemory = Map.delete name $ ctxMemory ctx }

-- store a variable
-- name (String) at the top of the stack ; contents (any type) as the next value
fct_store :: CoreFct
fct_store = do
  name <- pop
  value <- pop
  ifString name $ \n -> do
    let name = rmquotes n
    v <- lift . findvar $ n
    if isJust v
      then modify $ \ctx -> ctx { ctxMemory = Map.update (\_ -> Just value) name $ ctxMemory ctx }
      else modify $ \ctx -> ctx { ctxMemory = Map.insert name value $ ctxMemory ctx }

-- push all variable bindings to the stack
fct_showvars :: CoreFct
fct_showvars = modify $ \ctx -> ctx { ctxStack = (String . show . Map.toList $ ctxMemory ctx) : ctxStack ctx }

fct_base :: CoreFct
fct_base = do
  base <- pop
  ifInt base $ \b -> modify $ \ctx -> setCtxBase (Base (fromInteger b)) ctx

fct_showsettings :: CoreFct
fct_showsettings = modify $ \ctx -> ctx { ctxStack = (String . show $ ctxSettings ctx) : ctxStack ctx }

-- run the script (String) at the top of the stack
fct_run :: CoreFct
fct_run = do
  script <- pop
  ifString script $ \s -> do
    case parse $ rmquotes s of
      Left e -> throwError e
      Right xs -> mapM_ run xs

--
-- variable bindings
--

-- fetch the variable with the given name
findvar :: String -> State Context (Maybe Symbol)
findvar name = state $ \ctx -> (Map.lookup name $ ctxMemory ctx, ctx)


--
-- core stack operations
--

-- run the given string
--  - try to evaluate a function
--  - try to find a variable with this name
--  - push the associated symbol to the stack
run :: String -> ErrorT CalcError (State Context) ()
run x = do
  let s = stringToSymbol x
  case s of
    String _ -> do
      f' <- do -- try to find a variable with name x, return Nothing or an action that pushes the variable's value to the stack
        v <- lift . findvar $ x
        return $ liftM push v
      fromJust $ msum [findfct x, findoperator x, f', Just $ push s]
    _ -> push s

-- if the Symbol is a String, run the action, using the contents of the string as argument
-- otherwise, throw an error
--ifString :: MonadError CalcError m => Symbol -> (String -> m a) -> m a
ifString (String s) action = action s
ifString _ _ = throwError $ TypeMismatch "String"

ifInt (Int i) action = action i
ifInt _ _ = throwError $ TypeMismatch "Int"

-- main evaluator
calc :: String -> Context -> (Either CalcError (), Context)
calc args ctx = case parse args of
  Left e -> (Left e, ctx)
  Right s -> runState (runErrorT $ mapM_ run s) ctx

eval :: ErrorT CalcError (State Context) a -> Context -> (Either CalcError a, Context)
eval f s = runState (runErrorT f) s

parse :: String -> Either CalcError [String]
parse = glue [] (0,0)
  where glue [] (0,0) [] = return [] -- reached the end of the input and all blocks are closed? good parse
        glue xs (0,0) [] = return [reverse xs]
        glue [] (0,0) (' ':ys) = glue [] (0,0) ys -- skip extra spaces between arguments
        glue xs (0,0) (' ':ys) = (:) <$> pure (reverse xs) <*> glue [] (0,0) ys
        glue xs (a,b) (y:ys)
          | a' < 0    = throwError $ ParseError "Mismatched blocks"    -- a block has been closed that hadn't been opened before? no parse
          | otherwise = glue (y : xs) (a',b') ys
          where (a',b') = case y of
                  '(' -> (a+1, b)
                  ')' -> (a-1, b)
                  '"' -> (a  , 1-b)
                  _   -> (a  , b)
        glue _    _   [] = throwError $ ParseError "Mismatched blocks" -- reached the end of the input without closing all blocks? no parse

-- removes first and final quote, if present
rmquotes :: String -> String
rmquotes [] = []
rmquotes ss@(c:cs) = let cs' = reverse . rmquotes . reverse $ cs in if c == '"' then cs' else ss
