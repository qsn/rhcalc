module Core where

import Control.Monad.State (State)
import Control.Monad.Trans.Except (ExceptT)

import Stack (Stack, Context, CalcError)

contextFromStack :: Stack -> Context
st_dft :: Context
calc :: String -> Context -> (Maybe CalcError, Context)
type CoreFct = ExceptT CalcError (State Context) ()
