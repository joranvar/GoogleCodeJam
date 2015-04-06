module Y2010.Q.C where

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Solution Integer
                deriving Eq

instance Show Solution where
    show (Solution x) = show x

data Problem = Problem { rides :: Int
                       , capacity :: Int
                       , ts :: [Int]
                       } deriving (Eq,Show)

parse (rkn:gs:rest) =
    Problem _rides _capacity _queue : parse rest
        where [_rides:_capacity:_] = [map read $ words rkn]
              [_queue] = [map read $ words gs]
parse [] = []

solve (Problem _rides _capacity _queue) = Solution total
    where total = 0
