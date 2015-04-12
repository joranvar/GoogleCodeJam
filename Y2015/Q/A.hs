module Y2015.Q.A where

import Data.Char

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Solution Int
              deriving (Eq)
instance Show Solution
    where show (Solution x) = show x

data Problem = Problem { ss :: [Int]
                       } deriving (Eq, Show)

parse (line:rest) =
    let (_:ss:[]) = words line
    in Problem (map digitToInt ss) : parse rest
parse [] = []

solve (Problem ss) = Solution $ sum $ solve' ss 0 0

solve' :: [Int] -> Int -> Int -> [Int]
solve' [] _ _ = [0]
solve' (s:ss) i c
    | s == 0 && c <= i = 1 : solve' ss (i+1) (c+1)
    | s == 0 && c > i = 0 : solve' ss (i+1) c
    | otherwise = 0 : solve' ss (i+1) (c+s)
