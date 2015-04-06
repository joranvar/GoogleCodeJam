module Y2010.Q.B where

import Data.List

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Solution Integer
                deriving Eq

instance Show Solution where
    show (Solution x) = show x

data Problem = Problem { ts :: [Integer]
                       } deriving (Eq,Show)

parse (testCase:rest) =
    Problem _ts : parse rest
        where [_:_ts] = [map read $ words testCase]
parse [] = []

solve (Problem _ts) = Solution smallestStep
    where gcd' =
              foldl1 gcd $ map (\i -> i - minimum _ts) unique
                  where unique = (tail.sort.nub) _ts
          smallestStep
              | minimum _ts `mod` gcd' == 0 = 0
              | otherwise = gcd' - (minimum _ts `mod` gcd')
