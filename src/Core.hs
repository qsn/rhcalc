{-# LANGUAGE FlexibleContexts #-}
module Core
  (
    calc
  , contextFromStack
  , st_dft
  , CoreFct
  )
  where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import Data.Maybe

--import {-# SOURCE #-} Operators
import Operators
import Stack
import Parser (parse)

type CoreFct = ExceptT CalcError (State Context) ()


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
                        in modify $ modCtxMemory (Map.delete name)

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
      then modify $ modCtxMemory (Map.update (\_ -> Just value) name)
      else modify $ modCtxMemory (Map.insert name value)

-- push all variable bindings to the stack
fct_showvars :: CoreFct
fct_showvars = do
  ctx <- get
  push $ showvars (ctxMemory ctx)
  where showvars = String . show . Map.toList

fct_base :: CoreFct
fct_base = do
  base <- pop
  ifInt base $ \b -> modify $ \ctx -> setCtxBase (Base (fromInteger b)) ctx

fct_showsettings :: CoreFct
fct_showsettings = do
  ctx <- get
  push $ showsettings (ctxSettings ctx)
  where showsettings = String . show

-- run the script (String) at the top of the stack
fct_run :: CoreFct
fct_run = do
  script <- pop
  ifString script $ \s -> do
    case parse s of
      Left e -> throwE e
      Right xs -> mapM_ run xs

--
-- variable bindings
--

-- fetch the variable with the given name
findvar :: String -> State Context (Maybe Symbol)
findvar name = do
  ctx <- get
  return $ Map.lookup name $ ctxMemory ctx


--
-- core stack operations
--

-- run the given string
--  - try to evaluate a function
--  - try to find a variable with this name
--  - push the associated symbol to the stack
run :: Symbol -> ExceptT CalcError (State Context) ()
run s = do
  case s of
    String x -> do
      f' <- do -- try to find a variable with name x, return Nothing or an action that pushes the variable's value to the stack
        v <- lift . findvar $ x
        return $ liftM push v
      fromJust $ msum [findfct x, findoperator x, f', Just $ push s]
    _ -> push s


-- if the Symbol is a String, run the action, using the contents of the string as argument
-- otherwise, throw an error
--ifString :: MonadError CalcError m => Symbol -> (String -> m a) -> m a
ifString (String s) action = action s
ifString _ _ = throwE $ TypeMismatch "String"

ifInt (Int i) action
  | i > 1 = action i
ifInt _ _ = throwE $ TypeMismatch "need Int >= 1"

-- main evaluator
calc :: String -> Context -> (Maybe CalcError, Context)
calc args ctx = case parse args of
  Left e -> (Just e, ctx)
  Right s -> let (err, ctx') = runState (runExceptT $ mapM_ run s) ctx
             in (leftToMaybe err, ctx')

leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

eval :: ExceptT CalcError (State Context) a -> Context -> (Either CalcError a, Context)
eval f s = runState (runExceptT f) s

-- removes first and final quote, if present
rmquotes :: String -> String
rmquotes [] = []
rmquotes ss@(c:cs) = let cs' = reverse . rmquotes . reverse $ cs in if c == '"' then cs' else ss
