module Core where

import Control.Monad.State
import Control.Monad.Trans.Except

import Stack

contextFromStack :: Stack -> Context
st_dft :: Context
calc :: String -> Context -> (Maybe CalcError, Context)
type CoreFct = ExceptT CalcError (State Context) ()
