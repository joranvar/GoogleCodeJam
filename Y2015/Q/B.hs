module Y2015.Q.B where

import Control.Arrow
import Data.List
import Data.Ord

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Solution Int
              deriving (Eq)
instance Show Solution
    where show (Solution x) = show x

data Problem = Problem { pancakes :: [Int]
                       } deriving (Eq, Show)

parse (_:line:rest) =
    Problem (map read $ words line) : parse rest
parse [] = []

solve (Problem ps) = Solution $ solve' $ countStacks ps
    where countStacks = map (head &&& length) . group . sort

solve' :: [(Int, Int)] -> Int
solve' ps = uncurry (+) cheapestPaths
    where cheapestPath (iSofar, cSofar) (i, c) =
              minimumBy (comparing (uncurry (+)))
                            [ (iSofar + ((divisor-1) * c), max cSofar ((i`div`divisor) + (if i`mod`divisor > 0 then 1 else 0))) | divisor <- [1..i] ]
          cheapestPaths' start = foldl cheapestPath start ps
          cheapestPaths = cheapestPaths' (0, snd (cheapestPaths' (0,0))) -- Second run for minimum left anyway
