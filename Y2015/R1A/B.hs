{-# OPTIONS_GHC -O2 #-}
module Y2015.R1A.B where

import Data.List
import Data.Maybe
import Data.String.Utils
import Debug.Trace

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Solution Int
              deriving (Eq)
instance Show Solution
              where show (Solution x) = show x

data Problem = Problem { n :: Int
                       , ms :: [Int]
                       } deriving (Eq, Show)

parse (bn:ss:rest) =
    let ms = map read $ words ss
        [_,n] = map read $ words bn in
    Problem n ms : parse rest
parse [] = []

--solve p | trace (show p) False = undefined
solve (Problem n ms)
    | n <= length ms = Solution n
    | otherwise = Solution $ counted skipRepeat 0
      where skipRepeatTime = foldl1 lcm ms
            skipRepeat = 1 + ((n-1) `mod` sum (map (\m -> skipRepeatTime `div` m) ms))
            counted n time
                | n  > length ready = counted (n - helped) (time+1)
                | otherwise = ready !! (n-1) + 1
                where helped = length ready
                      ready = findIndices (\x -> time `mod` x == 0) ms
