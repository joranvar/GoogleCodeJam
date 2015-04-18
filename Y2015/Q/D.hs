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
    -- After reading the #1 solution in java by contestant kyc, I
    -- changed the following in order to pass the large test.  I have
    -- been wondering why, until I finally drew this figure and tried
    -- to fit it on a 3x5 board.
    --
    -- * *
    -- ***
    | x == 5 && r*c == 15 = Solution False
    | x == 1 = Solution True
    | r*c `mod` x /= 0 = Solution False
    | x > max r c = Solution False
    | x > 2 * min r c = Solution False
    | x >= 7 = Solution False
    | x > r*c = Solution False
    | x == 4 && min r c == 2 = Solution False
    | x == 6 &&  min r c < 4 = Solution False
    | otherwise = Solution True
