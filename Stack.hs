module Stack
  (
    Symbol(..)
  , Stack
  , Memory
  , Context
  , CalcError(..)
  , pop
  , push
  , tonum
  , dumpstack
  , stringToSymbol
  , symbolToString
  , symbolToString'
  , maybeToCalcError
  )
  where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map

import Data.Char
import Data.List

data Symbol = Int Integer | Frac (Integer,Integer) | Real Double | Bool Bool | Variable Char | String String | List [Symbol]
type Stack  = [Symbol]
type Memory = Map.Map String Symbol
type Context = (Stack, Memory)

data CalcError = ParseError String
               | TypeMismatch String
               | EmptyStack
               | OtherError String
               deriving (Show)

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
  show (Variable c) = [c]
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
          read_var  = if length s == 1 && isUpper (head s) then [(head s, [])] else []

dumpstack :: Stack -> String
dumpstack ys
  | len > 5   = '(' : show (len - 4) ++ " more on the stack)\n" ++ dump 4 ys
  | otherwise = dump 5 ys
  where len = length ys
        dump n ys = foldr ($) "" . reverse $ zipWith (\n y -> shows n . (':':) . (' ':) . shows y . ('\n':)) [1..n] ys

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
push x = modify $ \(xs, ys) -> (x:xs, ys)

-- pop a Symbol from the stack
-- fails if the stack is empty
pop :: ErrorT CalcError (State Context) Symbol
pop = do
  u <- get
  if null $ fst u
    then throwError EmptyStack
    else do
      modify h
      return $ head $ fst u
        where h (xs,ys) = (tail xs, ys)

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
