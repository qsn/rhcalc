module Operators
  (
    findoperator
  )
  where

import Control.Monad.State (modify)
import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR)
import Data.List (genericLength)
import qualified Data.Map as Map

import Stack (Symbol(Int, Frac, Real, Bool, Variable, String, List), Stack, ctxStack, modCtxStack, CalcError(OtherError), tonum)
import {-# SOURCE #-} Core  (calc, contextFromStack, CoreFct)

type HelpString = String
type Fct    = [Symbol] -> [Symbol]
{-
Stack operator
 - Number of inputs  (elements removed from the stack)
   * non-negative: the number of elements really removed from the stack
   * -1: removes a number of elements that depend on the parameters
   * -2: the operators uses the entire stack
 - Number of outputs (elements pushed back on the stack)
   * non-negative: the number of elements really pushed on the stack
   * -1: pushes a number of elements that depend on the parameters
 - Function to run
 - Description
-}
type Operator = (Int, Int, Fct, HelpString)

run :: Operator -> Stack -> Stack
run (argc,_,f,_) ys
  | length argv == n = f argv ++ zs
  where (n,ys') = case argc of
          -1 -> (round.tonum $ head ys, tail ys)
          -2 -> (length ys, ys)
          _ -> (argc, ys)
        (argv,zs) = splitAt n ys'


findoperator :: String -> Maybe CoreFct
findoperator name = modify . modCtxStack . run <$> Map.lookup name operators

op_neg :: Fct
op_neg [a] = case a of
  Int   n    -> [Int (-n)]
  Frac (n,d) -> [Frac (-n,d)]
  Real  x    -> [Real (-x)]

op_add :: Fct
op_add [a,b] = case (a,b) of
  (Int n,      Int n')       -> [Int  $ n+n']
  (Int n,      Frac (n',d')) -> do_frac (n,1) (n',d')
  (Int n,      Real x')      -> [Real $ (+) (fromIntegral n) x']
  (Frac (n,d), Int n')       -> do_frac (n,d) (n',1)
  (Frac (n,d), Frac (n',d')) -> do_frac (n,d) (n',d')
  (Frac (n,d), Real x')      -> [Real $ (+) ((fromIntegral n)/(fromIntegral d)) x']
  (Real x,     Int n')       -> [Real $ (+) x (fromIntegral n')]
  (Real x,     Frac (n',d')) -> [Real $ (+) x ((fromIntegral n')/(fromIntegral d'))]
  (Real x,     Real x')      -> [Real $ (+) x x']
  where do_frac (n,d) (n',d') = op_rnd $ [Frac (n*d' + n'*d, d*d')]

op_min :: Fct
op_min [b,a] = case (a,b) of
  (Int n,      Int n')       -> [Int  $ n-n']
  (Int n,      Frac (n',d')) -> do_frac (n,1) (n',d')
  (Int n,      Real x')      -> [Real $ (-) (fromIntegral n) x']
  (Frac (n,d), Int n')       -> do_frac (n,d) (n',1)
  (Frac (n,d), Frac (n',d')) -> do_frac (n,d) (n',d')
  (Frac (n,d), Real x')      -> [Real $ (-) ((fromIntegral n)/(fromIntegral d)) x']
  (Real x,     Int n')       -> [Real $ (-) x (fromIntegral n')]
  (Real x,     Frac (n',d')) -> [Real $ (-) x ((fromIntegral n')/(fromIntegral d'))]
  (Real x,     Real x')      -> [Real $ (-) x x']
  where do_frac (n,d) (n',d') = op_rnd $ [Frac (n*d' - n'*d, d*d')]

op_mult :: Fct
op_mult [a,b] = case (a,b) of
  (Int n,      Int n')       -> [Int  $ n*n']
  (Int n,      Frac (n',d')) -> do_frac (n,1) (n',d')
  (Int n,      Real x')      -> [Real $ (*) (fromIntegral n) x']
  (Frac (n,d), Int n')       -> do_frac (n,d) (n',1)
  (Frac (n,d), Frac (n',d')) -> do_frac (n,d) (n',d')
  (Frac (n,d), Real x')      -> [Real $ (*) ((fromIntegral n)/(fromIntegral d)) x']
  (Real x,     Int n')       -> [Real $ (*) x (fromIntegral n')]
  (Real x,     Frac (n',d')) -> [Real $ (*) x ((fromIntegral n')/(fromIntegral d'))]
  (Real x,     Real x')      -> [Real $ (*) x x']
  where do_frac (n,d) (n',d') = op_rnd $ [Frac (n*n',d*d')]

op_div :: Fct
op_div [b,a] = case (a,b) of
  (Int n,      Int n')       -> do_frac (n,1) (n',1)
  (Int n,      Frac (n',d')) -> do_frac (n,1) (n',d')
  (Int n,      Real x')      -> [Real $ (/) (fromIntegral n) x']
  (Frac (n,d), Int n')       -> do_frac (n,d) (n',1)
  (Frac (n,d), Frac (n',d')) -> do_frac (n,d) (n',d')
  (Frac (n,d), Real x')      -> [Real $ (/) ((fromIntegral n)/(fromIntegral d)) x']
  (Real x,     Int n')       -> [Real $ (/) x (fromIntegral n')]
  (Real x,     Frac (n',d')) -> [Real $ (/) x ((fromIntegral n')/(fromIntegral d'))]
  (Real x,     Real x')      -> [Real $ (/) x x']
  where do_frac (n,d) (n',d') = op_rnd $ [Frac (n*d',d*n')]

op_mod :: Fct
op_mod [Int b, Int a] = [Int $ mod a b]

op_pow :: Fct
op_pow [b,a] = case (a,b) of
  (Int n,      Int n')       -> [Int $ n^n']
  (Frac (n,d), Int n')       -> do_frac (n,d) n'
  (Int n,      Frac (n',d')) -> do_real (fromIntegral n) ((fromIntegral n')/(fromIntegral d'))
  (Int n,      Real x')      -> do_real (fromIntegral n) x'
  (Frac (n,d), Frac (n',d')) -> do_real ((fromIntegral n)/(fromIntegral d)) ((fromIntegral n')/(fromIntegral d'))
  (Frac (n,d), Real x')      -> do_real ((fromIntegral n)/(fromIntegral d)) x'
  (Real x,     Int n')       -> do_real x (fromIntegral n')
  (Real x,     Frac (n',d')) -> do_real x ((fromIntegral n')/(fromIntegral d'))
  (Real x,     Real x')      -> do_real x x'
  where do_frac (n,d) n' = op_rnd $ [Frac (n^n',d^n')]
        do_real x y      = [Real $ x**y]

op_inv :: Fct
op_inv [a] = case a of
  Int n      -> do_frac (n,1)
  Frac (n,d) -> do_frac (n,d)
  Real x     -> [Real $ 1/x]
  where do_frac (n,d) = op_rnd $ [Frac (d,n)]

op_num :: Fct
op_num [a] = case a of
  Int n      -> [Real $ fromIntegral n]
  Frac (n,d) -> [Real $ (fromIntegral n) / (fromIntegral d)]
  Real x     -> [Real x]
  List xs    -> [List $ map (\x -> head $ op_num [x]) xs]

op_rnd :: Fct
op_rnd [Int n]      = [Int n]
op_rnd [Frac (n,d)] = let (g,u,v) = (gcd n d, div n g, div d g) in if v == 1 then [Int u] else [Frac (u,v)]
op_rnd [Real x]     = [Int $ round x]
op_rnd [List xs]    = [List $ map (\x -> head $ op_rnd [x]) xs]

op_floor :: Fct
op_floor [Int n] = [Int n]
op_floor [Frac (n,d)] = [Int $ floor $ (intToDouble n)/(intToDouble d)]
op_floor [Real x] = [Int $ floor x]
op_floor [List xs]    = [List $ map (\x -> head $ op_floor [x]) xs]

op_ceil :: Fct
op_ceil [Int n] = [Int n]
op_ceil [Frac (n,d)] = [Int $ ceiling $ (intToDouble n)/(intToDouble d)]
op_ceil [Real x] = [Int $ ceiling x]
op_ceil [List xs]    = [List $ map (\x -> head $ op_ceil [x]) xs]

op_abs :: Fct
op_abs [Int n] = [Int $ abs n]
op_abs [Frac (n,d)] = [Frac (abs n,d)]
op_abs [Real x] = [Real $ abs x]
op_abs [List xs]    = [List $ map (\x -> head $ op_abs [x]) xs]

intToDouble :: Integer -> Double
intToDouble = fromIntegral

-- units/dimensions
data Dimension = Mass | Length | Time | Temperature | Volume | Pressure | Speed | Force deriving (Show,Eq) -- | Intensity | LightIntensity | ...
type Unit = (Dimension,(String,String))

--units_prefix = [("T", Int 1000000000000), ("P", Int 1000000000000000), ("G", Int 1000000000), ("M", Int 1000000), ("k",Int 1000),("h",Int 100),("m",Frac (1,1000)),("µ",Frac(1,1000000)),("n",Frac (1,1000000000)),("p",Frac (1,1000000000000)),("f",Frac (1,1000000000000000))]

units :: Map.Map String Unit
units = Map.fromList $ [
  ("m",(Length,("",""))), ("inch",(Length,("254 10000 / *","254 10000 / /"))), ("foot",(Length,("3048 10000 / *","3048 10000 / /"))),
  ("yd",(Length,("9144 10000 / *","9144 10000 / /"))), ("mile",(Length,("1609344 1000 / *", "1609344 1000 / /"))),
  ("l",(Volume,("",""))), ("gal",(Volume,("3785472 1000000 / *", "3785472 1000000 / /"))), ("floz",(Volume,("29574 1000000 / *", "29574 1000000 / /"))),
  ("Pa",(Pressure,("",""))), ("bar",(Pressure,("100000 *","100000 /"))), ("psi",(Pressure,("6894.757 *", "6894.757 /"))),
  ("kg",(Mass,("",""))),("lb",(Mass,("0.45359237 *","0.45359237 /"))),
  ("k",(Temperature,("",""))), ("dC",(Temperature,("273.15 +","273.15 -"))), ("dF",(Temperature,("459.67 + 5 * 9 /","9 * 5 / 459.67 -")))
  ]

unit_get :: String -> Either CalcError (String,Unit)
unit_get unit = case Map.lookup unit units of
  Just u  -> Right (unit,u)
  Nothing -> Left $ OtherError $ "unit not found " ++ unit

unit_scriptfrom :: (a, (b, c)) -> b
unit_scriptfrom = fst.snd
unit_scriptto   :: (a, (b, c)) -> c
unit_scriptto   = snd.snd

unit_convert :: Fct
unit_convert [String to, String from, value] = case dims of
  Left err -> [String $ show err]
  Right _  -> eitherToStack $ unitPair >>= convert
  where unit_from :: Either CalcError (String, Unit)
        unit_from = unit_get from
        unit_to   :: Either CalcError (String, Unit)
        unit_to   = unit_get to
        dims :: Either CalcError (Unit, Unit)
        dims = unitPair >>= check_dims
        check_dims :: ((String,Unit),(String,Unit)) -> Either CalcError (Unit,Unit)
        check_dims (a,b) = check_dimensions a b
        unitPair :: Either CalcError ((String,Unit),(String,Unit))
        unitPair = do
          uf <- unit_from
          ut <- unit_to
          return (uf,ut)
        convert :: ((String,Unit),(String,Unit)) -> Either CalcError Stack
        convert (u_from, u_to) = runscript script [value]
          where script = unit_scriptfrom (snd u_from) ++ " " ++ unit_scriptto (snd u_to)

-- TODO add dimension equivalence for compound dimensions such as Speed, Force or Volume
check_dimensions :: (String,Unit) -> (String,Unit) -> Either CalcError (Unit, Unit)
check_dimensions (str1,unit1) (str2,unit2) = if   dim unit1 == dim unit2
                                             then Right (unit1, unit2)
                                             else Left $ OtherError $ "units " ++ str1 ++ " and " ++ str2 ++ " don't match (" ++ (show $ dim unit1) ++ "/" ++ (show $ dim unit2) ++ ")"
  where dim unit = fst unit

op_unit = [("convert",(3,1,unit_convert, "Unit converter"))]


-- a script here is supposed to contain only basic operators, not core functions, so it has no effect on variables, only the stack
runscript :: String -> Stack -> Either CalcError Stack
runscript script stack = h $ calc script $ contextFromStack stack
  where h (Just err, _) = Left err
        h (Nothing, ctx) = Right $ ctxStack ctx

eitherToStack :: Either CalcError Stack -> Stack
eitherToStack (Left err) = [String $ show err]
eitherToStack (Right ss) = ss

-- lists
op_sum :: [Symbol] -> Stack
op_sum [List xs] = let (n,ps) = (length xs,concat $ replicate (n-1) "+ ") in eitherToStack $ runscript ps xs
op_mean :: [Symbol] -> Stack
op_mean [List xs] = let (n,ps) = (length xs,concat $ replicate (n-1) "+ ") in eitherToStack $ runscript (ps ++ show n ++ " /") xs

op_concat, op_addhead, op_reverse, op_length :: Fct
op_head, op_tail, op_last, op_init :: Fct
op_drop, op_take, op_range :: Fct
op_concat [List   b,List   a] = [List   $ a ++ b]
op_concat [String b,String a] = [String $ a ++ b]
op_addhead [List xs, x] = [List $ x : xs]
op_addhead [x, List xs] = [List $ x : xs]
op_reverse [List   a] = [List   $ reverse a]
op_reverse [String a] = [String $ reverse a]
op_length  [List   a] = [Int $ genericLength a]
op_length  [String a] = [Int $ genericLength a]
op_head [List xs] = [head xs]
op_head [String xs] = [String $ head xs : []]
op_tail [List xs] = [List $ tail xs]
op_tail [String xs] = [String $ tail xs]
op_last [List xs] = [last xs]
op_last [String xs] = [String $ last xs : []]
op_init [List xs] = [List $ init xs]
op_init [String xs] = [String $ init xs]
op_drop [Int n, List xs] = [List $ drop (fromIntegral n) xs]
op_drop [Int n, String xs] = [String $ drop (fromIntegral n) xs]
op_take [Int n, List xs] = [List $ take (fromIntegral n) xs]
op_take [Int n, String xs] = [String $ take (fromIntegral n) xs]
op_range [end,start] = case (start,end) of
  (Int a,Int b) -> [List $ map Int [a..b]]
  (  _  ,  _  ) -> [List $ map Real [(tonum start)..(tonum end)]]

-- stack
op_swap, op_del, op_dup, op_rep, op_get, op_clear, op_type :: Fct
op_swap [a,b] = [b,a]
op_del  [_]   = []
op_dup  [a]   = [a,a]
op_rep  [Int n,a]  = replicate (fromIntegral n) a
op_get  xs    = last xs : init xs
op_clear _ = []
op_type [a]   = case a of
  Int      _ -> [String "Integer",  a]
  Frac     _ -> [String "Frac",     a]
  Real     _ -> [String "Real",     a]
  Bool     _ -> [String "Bool",     a]
  Variable _ -> [String "Variable", a]
  String   _ -> [String "String",   a]
  List     _ -> [String "List",     a]


-- logic/boolean operators
logic_and, logic_or, logic_xor :: Fct
logic_not, logic_nor, logic_nand :: Fct
logic_lshift, logic_rshift :: Fct
logic_swap2, logic_swap4, logic_swap8 :: Fct
logic_and [Bool a, Bool b] = [Bool $ a && b]
logic_and [Int a,  Int b]  = [Int  $ a .&. b]
logic_or  [Bool a, Bool b] = [Bool $ a || b]
logic_or  [Int a,  Int b]  = [Int  $ a .|. b]
logic_xor [Bool a, Bool b] = [Bool $ (a || b) && not (a && b)]
logic_xor [Int a,  Int b]  = [Int  $ a `xor` b]
logic_not [Bool a]         = [Bool $ not a]
logic_nor  = logic_not . logic_or
logic_nand = logic_not . logic_and
logic_lshift [Int n, Int a]
  | a >= 0 && n >= 0 = [Int $ shiftL a (fromInteger n)]
logic_rshift [Int n, Int a]
  | a >= 0 && n >= 0 = [Int $ shiftR a (fromInteger n)]
logic_swap2 [Int a]
  | a >= 0 = [Int $ do_swap 2 a]
logic_swap4 [Int a]
  | a >= 0 = [Int $ do_swap 4 a]
logic_swap8 [Int a]
  | a >= 0 = [Int $ do_swap 8 a]

do_swap :: Int -> Integer -> Integer
do_swap bytes = foldl (.|.) 0 . zipWith (flip shiftLB) [0..] . as_bytes bytes
  where shiftLB x = shiftL x . (*8)
        shiftRB x = shiftR x . (*8)
        as_bytes b x = map h $ reverse [0..b-1]
          where h i = (x `shiftRB` i) .&. 0xff

-- comparison tests
test_eq, test_ne, test_lt, test_gt, test_le, test_ge :: Ord a => [a] -> [Symbol]
test_eq [b,a] = [Bool $ a == b]
test_ne [b,a] = [Bool $ a /= b]
test_lt [b,a] = [Bool $ a <  b]
test_gt [b,a] = [Bool $ a >  b]
test_le [b,a] = [Bool $ a <= b]
test_ge [b,a] = [Bool $ a >= b]

-- math functions
mathfct :: (Double -> Double) -> [Symbol] -> [Symbol]
mathfct f [a] = case a of
  Int n      -> [Real (f $ fromIntegral n)]
  Frac (n,d) -> [Real (f $ (fromIntegral n)/(fromIntegral d))]
  Real x     -> [Real (f $ x)]

operators :: Map.Map String Operator
operators   = Map.fromList $ op_stack ++ op_math ++ op_fct ++ op_logic ++ op_test ++ op_list ++ cst {-++ op_abstract-} ++ op_unit
cst, op_list, op_stack, op_math, op_fct, op_logic, op_test, op_unit :: [(String, Operator)]
cst         = [("pi", (0,1,\_ -> [Real pi], "Pi constant"))]
op_list     = [("++", (2,1,op_concat, "Concatenate two lists")),
               (":", (2,1,op_addhead, "Add an element at the head of a list")),
               ("reverse", (1,1,op_reverse, "Reverse a list")),
               ("length", (1,1,op_length, "Length of a list")),
               ("head", (1,1,op_head, "Head of a list")),
               ("tail", (1,1,op_tail, "Tail of a list")),
               ("last", (1,1,op_last, "Last element of a list")),
               ("init", (1,1,op_init, "First elements of a list")),
               ("drop", (2,1,op_drop, "Drop the first n elements from a list")),
               ("take", (2,1,op_take, "Take the first n elements from a list")),
               ("range", (2,1,op_range, "Creates a range of numbers")),
               ("sum", (1,1,op_sum, "Sum all elements in a list")),
               ("mean", (1,1,op_mean, "Computes the mean value of a list"))]
op_stack    = [("swap", (2,2,op_swap, "Swap the first two elements on the stack")),
               ("del", (1,0,op_del, "Remove the first element from the stack")),
               ("dup", (1,2,op_dup, "Duplicate the first stack element")),
               ("rep", (2,-1,op_rep, "Repeat the second element n times, where n is the first element on the stack")),
               ("get", (-1,-1,op_get, "Puts the n-th element at the top of the stack, where n is the first element on the stack")),
               ("cls", (-2,0,op_clear, "Clears the stack")),
               ("type", (1,2,op_type, "Prints the type of the first stack element"))]
op_math     = [("n", (1,1,op_neg, "Changes the sign of the first stack element")),
               ("+", (2,1,op_add, "Adds the first two elements")),
               ("-", (2,1,op_min, "Substracts the first element from the second")),
               ("*", (2,1,op_mult, "Multiplies the first two elements")),
               ("/", (2,1,op_div, "Divides the second element by the first")),
               ("%", (2,1,op_mod, "Remainder of the division of the second element by the first")),
               ("^", (2,1,op_pow, "Elevates the second element to the first")),
               ("inv", (1,1,op_inv, "Inverses the first element on the stack")),
               ("num", (1,1,op_num, "Replaces the first element by its numerical value")),
               ("rnd", (1,1,op_rnd, "Rounds a numerical value")),
               ("floor", (1,1,op_floor, "Rounds a numerical value to the integer immediately smaller")),
               ("ceil", (1,1,op_ceil, "Rounds a numerical value to the integer immediately larger")),
               ("abs", (1,1,op_abs, "Absolute value"))]
op_fct      = [("cos", (1,1,mathfct cos, "Cosine, parameter in radians")),
               ("sin", (1,1,mathfct sin, "Sine, parameter in radians")),
               ("tan", (1,1,mathfct tan, "Tangent, parameter in radians")),
               ("exp", (1,1,mathfct exp, "Exponential")),
               ("log", (1,1,mathfct log, "Logarithm (base e))")),
               ("sqrt", (1,1,mathfct sqrt, "Square root"))]
op_logic    = [("and", (2,1,logic_and, "Logical AND")),
               ("or", (2,1,logic_or, "Logical OR")),
               ("xor", (2,1,logic_xor, "Logical XOR")),
               ("nand", (2,1,logic_nand, "Logical NAND")),
               ("not", (1,1,logic_not, "Logical NOT")),
               ("nor", (2,1,logic_nor, "Logical NOR")),
               ("<<", (2,1,logic_lshift, "Bitwise left shift")),
               (">>", (2,1,logic_rshift, "Bitwise right shift")),
               ("swap2", (1,1,logic_swap2, "Swap two-byte int")),
               ("swap4", (1,1,logic_swap4, "Swap four-byte int")),
               ("swap8", (1,1,logic_swap8, "Swap eight-byte int"))]
op_test     = [("==", (2,1,test_eq, "Equality test")),
               ("<", (2,1,test_lt, "Inequality test, true if second < first")),
               (">", (2,1,test_gt, "Inequality test, true if second > first")),
               ("/=", (2,1,test_ne, "Difference test")),
               ("<=", (2,1,test_le, "Inequality test, true if second <= first")),
               (">=", (2,1,test_ge, "Inequality test, true if second >= first"))]
{-
op_abstract = [("solve", (1,1,abs_solve, "Simple solver"))]

abs_solve [String eq] = case parse $ rmquotes eq of
  Just [a, [_], "*", b, "+"] -> let expr = b ++ " " ++ a ++ " / n" in fst $ calc expr ([],Map.empty)
  Just [a, [_], "*", b, "-"] -> let expr = b ++ " " ++ a ++ " /"   in fst $ calc expr ([],Map.empty)
  Nothing -> [String "no parse", String eq]
  _ -> [String $ "invalid/unsolvable equation: " ++ eq, String eq]
-}
