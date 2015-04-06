module Y2010.Q.A where

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = OFF
              | ON
                deriving Eq

instance Show Solution where
    show OFF = "OFF"
    show ON  = "ON"

data Problem = Problem { n :: Int
                       , k :: Int
                       } deriving (Eq,Show)

parse (nk:rest) =
    Problem _n _k : parse rest
        where [_n:_k:_] = [map read $ words nk]
parse [] = []

solve (Problem _n _k)
    | (1 + _k) `mod` (2 ^ _n) == 0 = ON
    | otherwise = OFF
