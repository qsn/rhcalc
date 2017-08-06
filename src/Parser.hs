{-# LANGUAGE TupleSections #-}
module Parser
  (
    parse
  )
  where

import Stack (CalcError(ParsecError), Symbol(String, Bool, Variable, Real, Int, List))

import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)

import Data.Functor ((<$>))
import Control.Monad (when)
import Numeric (readHex, readOct, readDec, readInt)

parse :: [Char] -> Either CalcError [Symbol]
parse input = case Parsec.parse rpnInput "(RPN input)" input of
  Left err -> Left $ ParsecError err
  Right res -> Right res

rpnInput :: GenParser Char st [Symbol]
rpnInput = do
  spaces
  result <- fmap concat (sepEndBy command (many1 space))
  eof
  return result

-- functions and operators identified by proper text are handled as strings
command :: GenParser Char st [Symbol]
command = combined
      <|> fmap (:[]) operator

combined :: GenParser Char st [Symbol]
combined = do
  lit <- literal
  op <- optionMaybe operator
  case op of
    Nothing -> return [lit]
    Just o  -> return [lit, o]

operator :: GenParser Char st Symbol
operator = fmap String $ many1 (oneOf symbolChar)

symbolChar :: [Char]
symbolChar = "+-/*%^<>=|&!~@#$:,"

literal :: GenParser Char st Symbol
literal = number
      <|> fmap (Bool . read) bool
      <|> fmap Variable variable
      <|> list
      <|> fmap String quotedString
      <|> fmap String unquotedString
      <?> "value"

separator :: GenParser Char st Char
separator = lookAhead . try . oneOf $ ' ' : '[' : ']' : symbolChar

complete :: GenParser Char st Char
complete = eof >> return '\0'

manyM :: GenParser Char st a -> GenParser Char st [a]
manyM p = manyTill p (separator <|> complete)

variable :: GenParser Char st String
variable = try $ do
  x <- upper
  xs <- manyM alphaNum
  return $ x:xs

bool :: GenParser Char st String
bool = try (string "True" <|> string "False")

number :: GenParser Char st Symbol
number = fmap (Real . read) real <|> wrap <$> int
  where wrap = Int . fst . head

real :: GenParser Char st String
real = try $ do
  intPart <- many digit
  char '.'
  let (intPart', combinator) = if null intPart then ("0", many1) else (intPart, many)
  decPart <- combinator digit
  return $ intPart' ++ '.' : decPart

int :: GenParser Char st [(Integer, String)]
int =  try hexInt <|> try octalInt <|> try binInt <|> try decInt

intBase :: GenParser Char st Char -> ReadS Integer -> String -> GenParser Char st [(Integer, String)]
intBase p reader err = do
  i <- manyM p
  when (null i) $ fail err
  return $ reader i

hexInt   :: GenParser Char st [(Integer,String)]
octalInt :: GenParser Char st [(Integer,String)]
binInt   :: GenParser Char st [(Integer,String)]
decInt   :: GenParser Char st [(Integer,String)]
hexInt = string "0x" >> intBase hexDigit readHex "hexadecimal number"
octalInt = char '0' >> intBase octDigit readOct "octal number"
binInt = string "0b" >> intBase (oneOf "01") readBin "binary number"
  where readBin :: Num a => ReadS a
        readBin = readInt 2 (`elem` "01") (\c -> if c == '0' then 0 else 1)
decInt = intBase digit readDec "number"

quotedString :: GenParser Char st String
quotedString = do
  char '"'
  c <- many1 quotedChar
  char '"'
  return c
  where quotedChar = try escaped <|> noneOf "\""
        escaped = string "\\\"" >> return '\"' :: GenParser Char st Char

unquotedString :: GenParser Char st String
unquotedString = many1 unquotedChar
  where unquotedChar = try escaped <|> alphaNum <|> oneOf "_."
        escaped = char '\\' >> oneOf "\' " :: GenParser Char st Char


list :: GenParser Char st Symbol
list = List <$> between (char '[') (char ']') (sepBy listItem (char ','))
listItem :: GenParser Char st Symbol
listItem = do
  spaces
  i <- literal
  spaces
  return i
