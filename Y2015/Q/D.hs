module Y2015.Q.D where

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Solution Bool
              deriving (Eq)
instance Show Solution
    where show (Solution True) = "GABRIEL"
          show (Solution False) = "RICHARD"

data Problem = Problem { x :: Int
                       , r :: Int
                       , c :: Int
                       } deriving (Eq, Show)

parse (xrc:rest) =
    let (x:r:c:[]) = map read $ words xrc in
    Problem x r c : parse rest
parse [] = []

solve (Problem x r c)
    | x == 1 = Solution True
    | r*c `mod` x /= 0 = Solution False
    | x > max r c = Solution False
    | x > 2 * min r c = Solution False
    | x >= 7 = Solution False
    | x > r*c = Solution False
    | x == 4 && min r c == 2 = Solution False
    | x == 6 &&  min r c < 4 = Solution False
    | otherwise = Solution True
