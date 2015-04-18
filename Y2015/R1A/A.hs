module Y2015.R1A.A where

import Data.List
import Data.Maybe
import Data.String.Utils

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Solution Int Int
              deriving (Eq)
instance Show Solution
    where show (Solution y z) = show y ++ " " ++ show z

data Problem = Problem { xs :: [Int]
                       } deriving (Eq, Show)

parse (_:ss:rest) =
    let xs = map read $ words ss in
    Problem xs : parse rest
parse [] = []

solve (Problem xs) = Solution eatEmAll eatConstantly
    where eatEmAll = sum eatings
          eatConstantly = sum $ map (min (maximum eatings)) $ init xs
          eatings = filter (>=0) . zipWith (-) xs $ tail xs
