module Y2014.R1A.A where

import Data.BitVector
import qualified Data.Set as Set

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = NotPossible
              | Switches Int
                deriving Eq

instance Show Solution where
    show NotPossible  = "NOT POSSIBLE"
    show (Switches c) = show c

data Problem = Problem { outlets :: [BV]
                       , devices :: [BV]
                       } deriving (Eq,Show)

parse (nl:ns:ls:rest) =
    Problem
    (map fromBits . map toBools $ words ns)
    (map fromBits . map toBools $ words ls)
    : parse rest
parse [] = []

toBools :: [Char] -> [Bool]
toBools = map toBool
    where toBool '1' = True
          toBool '0' = False

solve p@(Problem out dev) = 
    case possibleSwitchStrings p of
      [] -> NotPossible
      ss -> case matchSwitchStrings p ss of
              [] -> NotPossible
              ms -> Switches (minimum (map (foldl_ countTrue 0) ms))
                    where countTrue i True  = 1+i
                          countTrue i False = i

matchSwitchStrings :: Problem -> [BV] -> [BV]
matchSwitchStrings p@(Problem out dev) (x:xs)
    | all (flip Set.member (Set.fromList out)) $ map (xor x) dev = x : matchSwitchStrings p xs
    | otherwise                                                  = matchSwitchStrings p xs
matchSwitchStrings _ [] = []

possibleSwitchStrings :: Problem -> [BV]
possibleSwitchStrings (Problem out dev) =
    map (switch (head dev)) out

switch :: BV -> BV -> BV
switch = xor
