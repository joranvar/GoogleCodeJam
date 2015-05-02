module Y2015.R1A.C where

import Data.List
import Data.Maybe
import Data.String.Utils
import Debug.Trace

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Solution [Int]
              deriving (Eq)
instance Show Solution
              where show (Solution x) = '\n' : unlines (map show x)

data Problem = Problem { trees :: [(Int, Int)]
                       } deriving (Eq, Show)

parse (nline:rest) =
    let n = read nline
        mapCoords l = let (x:y:[]) = map read $ words l in (x,y) :: (Int, Int)
    in Problem (map mapCoords (take n rest)) : parse (drop n rest)
parse [] = []

--solve p | trace (show p) False = undefined
solve (Problem trees) = Solution $ replicate 5 0
