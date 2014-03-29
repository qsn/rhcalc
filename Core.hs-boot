module Core where

import Control.Monad.State
import Control.Monad.Error

import Stack

contextFromStack :: Stack -> Context
st_dft :: Context
calc :: String -> Context -> (Either CalcError (), Context)
type CoreFct = ErrorT CalcError (State Context) ()
