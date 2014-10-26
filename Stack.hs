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
  , dumpstack
  , stringToSymbol
  , symbolToString
  , symbolToString'
  , maybeToCalcError
  , defaultSettings
  , ctxBase
  , setCtxBase
  , modCtxStack
  , modCtxMemory
  )
  where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec.Error as PE (ParseError)

import Data.Char
import Data.List
import Numeric (showIntAtBase)

data Symbol = Int Integer | Frac (Integer,Integer) | Real Double | Bool Bool | Variable String | String String | List [Symbol]
type Stack  = [Symbol]
type Memory = Map.Map String Symbol
data AngleUnit = Rad | Deg deriving (Show,Eq)
newtype Base = Base Int deriving (Eq)
data Settings = Settings { setBase :: Base, setAngle :: AngleUnit }
data Context = Context { ctxStack :: Stack, ctxMemory :: Memory, ctxSettings :: Settings }

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

data CalcError = ParseError String
               | ParsecError (PE.ParseError)
               | TypeMismatch String
               | EmptyStack
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

instance Error CalcError where
  noMsg = OtherError "Unknown error"
  strMsg s = OtherError s

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
  compare (String s) _ = undefined
  compare _ (String s) = undefined
  compare (Variable c) _ = undefined
  compare _ (Variable c) = undefined
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

instance Read Symbol where
  readsPrec _ s
    | not.null $ read_int  = [(Int  (fst.head $ read_int), snd.head $ read_int)]
    | not.null $ read_frac = [(Frac (fst.head $ read_frac), snd.head $ read_frac)]
    | not.null $ read_real = [(Real (fst.head $ read_real), snd.head $ read_real)]
    | not.null $ read_bool = [(Bool (fst.head $ read_bool), snd.head $ read_bool)]
    | not.null $ read_list = [(List (fst.head $ read_list), snd.head $ read_list)]
    | not.null $ read_var = [(Variable (fst.head $ read_var), snd.head $ read_var)]
    | otherwise            = [(String s, "")]
    where read_int  = reads s :: [(Integer,String)]
          read_frac = reads s :: [((Integer,Integer),String)]
          read_real = reads s :: [(Double,String)]
          read_bool = reads s :: [(Bool,String)]
          read_list = reads s :: [([Symbol],String)]
          read_var  = if length s >= 1 && isUpper (head s) then [(s, [])] else [] -- not quite right

dumpstack :: Base -> Stack -> String
dumpstack base ys
  | len > 5   = '(' : show (len - 4) ++ " more on the stack)\n" ++ dump 4 ys
  | otherwise = dump 5 ys
  where len = length ys
        dump n ys = foldr ($) "" . reverse $ zipWith (\n y -> shows n . (':':) . (' ':) . showsSymbol y . ('\n':)) [1..n] ys
        showsSymbol s = case base of
          Base 10 -> shows s
          Base b  -> case s of
            Int i -> showIntAtBase (fromIntegral b) intToDigit i
            _     -> shows s

peek :: Stack -> String
peek ys = show $ head ys

tonum :: Symbol -> Double
tonum (Int n)      = fromIntegral n
tonum (Frac (n,d)) = (fromIntegral n)/(fromIntegral d)
tonum (Real x)     = x
tonum (Bool True)  = 1
tonum (Bool False) = 0


-- push a Symbol to the stack
-- never fails
push :: Symbol -> ErrorT CalcError (State Context) ()
push x = modify $ modCtxStack (x:)

-- pop a Symbol from the stack
-- fails if the stack is empty
pop :: ErrorT CalcError (State Context) Symbol
pop = do
  ctx <- get
  if null $ ctxStack ctx
    then throwError EmptyStack
    else do
      let (x:xs) = ctxStack ctx
      put $ ctx { ctxStack = xs }
      return x


stringToSymbol :: String -> Symbol
stringToSymbol x = case reads x :: [(Symbol, String)] of
  [(a, "")] -> a
  _ -> String x

symbolToString = eitherToMaybe . symbolToString'
symbolToString' (String s) = return s
symbolToString' _ = Left $ TypeMismatch "String"
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left  b) = Nothing
eitherToMplus (Right a) = return a
eitherToMplus (Left b)  = mzero
maybeToCalcError (Just x) = Right x
maybeToCalcError Nothing  = Left $ OtherError "Unknown error"
