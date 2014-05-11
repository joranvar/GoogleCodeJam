module Y2014.R1C.A where

import Data.Ratio
import qualified Data.Set as Set

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = NotPossible
              | Generations Int
                deriving Eq

instance Show Solution where
    show NotPossible  = "impossible"
    show (Generations c) = show c

data Problem = Problem Rational deriving (Eq,Show)

parse [] = []
parse (line:rest) = Problem (read p % read (tail q)) : parse rest
    where (p,q) = span (/='/') line

solve (Problem r)
    | denominator (r * (2^40)) > 1 = NotPossible
    | otherwise             =  Generations (abs . floor $ logBase 2 (fromRational r))


