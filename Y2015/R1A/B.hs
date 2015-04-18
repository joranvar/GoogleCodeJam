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

data Problem = Problem { b :: Int
                       , n :: Int
                       , ms :: [Int]
                       } deriving (Eq, Show)

parse (bn:ss:rest) =
    let ms = map read $ words ss
        (b:n:[]) = map read $ words bn in
    Problem b n ms : parse rest
parse [] = []

--solve p | trace (show p) False = undefined
solve (Problem b n ms)
    | n <= length ms = Solution n
    | otherwise = Solution $ counted skipRepeat (replicate (length ms) 0)
      where skipRepeatTime = foldl1 lcm ms
            skipRepeat = 1 + ((n-1) `mod` (sum $ map (\m -> skipRepeatTime `div` m) ms))
            counted n busy
                | n  > length ready = counted (n - helped) newbusy
                | otherwise = ready !! (n-1) + 1
                where helped = length ready
                      newbusy = zipWith assignCustomer [0..] busy
                      nextTime = minimum busy
                      ready = elemIndices nextTime busy
                      assignCustomer i barberTime = if barberTime == nextTime
                                                          then barberTime + ms !! i
                                                          else barberTime
