module Operators where

import Stack

type HelpString = String
type Fct    = [Symbol] -> [Symbol]
type Operator = (Int, Int, Fct, HelpString)

run :: Operator -> Stack -> Stack

findoperator :: String -> Maybe Operator
