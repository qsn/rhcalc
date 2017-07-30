module Stack
  (
    Symbol(..)
  , Stack
  , Context(..)
  , Base(Base)
  , CalcError(..)
  , pop
  , push
  , tonum
  , dumpcontext
  , defaultSettings
  , ctxBase
  , setCtxBase
  , modCtxStack
  , modCtxMemory
  , showCtxMemory
  , showCtxSettings
  )
  where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.State (State, modify, get, put)
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec.Error as PE (ParseError)

import Data.Char (intToDigit)
import Numeric (showIntAtBase)

data Symbol = Int Integer | Frac (Integer,Integer) | Real Double | Bool Bool | Variable String | String String | List [Symbol]
type Stack  = [Symbol]
type Memory = Map.Map String Symbol
data AngleUnit = Rad | Deg deriving (Show,Eq)
newtype Base = Base Int deriving (Eq)
data Settings = Settings { setBase :: Base, setAngle :: AngleUnit }
data Context = Context { ctxStack :: Stack, ctxMemory :: Memory, ctxSettings :: Settings }

defaultSettings :: Settings
defaultSettings = Settings { setBase = Base 10, setAngle = Rad }

ctxBase :: Context -> Base
ctxBase = setBase . ctxSettings

setCtxBase :: Base -> Context -> Context
setCtxBase b ctx = let settings = ctxSettings ctx
                   in ctx { ctxSettings = settings { setBase = b } }

modCtxStack :: (Stack -> Stack) -> Context -> Context
modCtxStack f ctx = ctx { ctxStack = f (ctxStack ctx) }

modCtxMemory :: (Memory -> Memory) -> Context -> Context
modCtxMemory f ctx = ctx { ctxMemory = f (ctxMemory ctx) }

showCtxSettings :: Context -> String
showCtxSettings = show . ctxSettings

showCtxMemory :: Context -> String
showCtxMemory = show . Map.toList . ctxMemory

data CalcError = ParseError String
               | ParsecError (PE.ParseError)
               | TypeMismatch String
               | EmptyStack
               | OperationNotSupported
               | OtherError String
               deriving (Show)

instance Show Base where
  showsPrec _ (Base b) = case b of
    2  -> ("Binary" ++)
    8  -> ("Octal" ++)
    10 -> ("Decimal" ++)
    16 -> ("Hex" ++)
    _  -> ("Base " ++) . (show b ++)

instance Show Settings where
  showsPrec _ s = shows (setBase s) . (' ':) . shows (setAngle s)

instance Eq Symbol where
  (==) (Int n) (Int n') = n == n'
  (==) (Int n) (Frac (n',d')) = (==) (Frac (n,1)) (Frac (n',d'))
  (==) (Frac (n,d)) (Int n')  = (==) (Frac (n,d)) (Frac (n',1))
  (==) (Frac (n,d)) (Frac (n',d')) = (==) (n*d') (n'*d)
  (==) (Bool b) (Bool b')     = (==) b b'
  (==) (Bool _)    _          = False
  (==)    _     (Bool _)      = False
  (==) (String s) (String s') = (==) s s'
  (==) (String _) _           = False
  (==) _          (String _)  = False
  (==) (Variable x) (Variable y) = x == y
  (==) (Variable _)   _          = undefined
  (==)    _         (Variable _) = undefined
  (==) (List xs) (List ys)    = (==) xs ys
  (==) (List _)   _           = undefined
  (==)   _  (List _)          = undefined
  (==) x y = (==) (tonum x) (tonum y)

instance Ord Symbol where
  compare (Int n) (Int n') = compare n n'
  compare (Int n) (Frac (n',d')) = compare (Frac (n,1)) (Frac (n',d'))
  compare (Frac (n,d)) (Int n')  = compare (Frac (n,d)) (Frac (n',1))
  compare (Frac (n,d)) (Frac (n',d')) = compare (n*d') (n'*d)
  compare (String s) (String s') = compare s s'
  compare (String _) _ = undefined
  compare _ (String _) = undefined
  compare (Variable _) _ = undefined
  compare _ (Variable _) = undefined
  compare (List xs) (List ys) = compare xs ys
  compare (List _)   _        = undefined
  compare   _  (List _)       = undefined
  compare x y = compare (tonum x) (tonum y)

instance Show Symbol where
  show (Int n)      = show n
  show (Frac (n,d)) = show n ++ '/' : show d
  show (Real x)     = show x
  show (Bool b)     = show b
  show (String s)   = s
  show (Variable s) = s
  show (List xs)    = show xs

dumpcontext :: Context -> String
dumpcontext ctx = dumpstack (ctxBase ctx) (ctxStack ctx)

dumpstack :: Base -> Stack -> String
dumpstack base ys
  | len > 5   = '(' : show (len - 4) ++ " more on the stack)\n" ++ dump 4 ys
  | otherwise = dump 5 ys
  where len = length ys
        dump n xs = foldr ($) "" . reverse $ zipWith (\i x -> shows i . (':':) . (' ':) . showsSymbol x . ('\n':)) [1..n] xs
        showsSymbol s = case base of
          Base 10 -> shows s
          Base b  -> case s of
            Int i -> (if i < 0 then ('-':) else id) . showIntAtBase (fromIntegral b) intToDigit (abs i)
            _     -> shows s

tonum :: Symbol -> Double
tonum (Int n)      = fromIntegral n
tonum (Frac (n,d)) = (fromIntegral n)/(fromIntegral d)
tonum (Real x)     = x
tonum (Bool True)  = 1
tonum (Bool False) = 0
tonum _ = error "Can't convert this to a number"


-- push a Symbol to the stack
-- never fails
push :: Symbol -> ExceptT CalcError (State Context) ()
push x = modify $ modCtxStack (x:)

-- pop a Symbol from the stack
-- fails if the stack is empty
pop :: ExceptT CalcError (State Context) Symbol
pop = do
  ctx <- get
  case ctxStack ctx of
    [] -> throwE EmptyStack
    (x:xs) -> do
      put $ ctx { ctxStack = xs }
      return x
