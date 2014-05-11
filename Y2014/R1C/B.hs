module Y2014.R1C.B where

import Control.Monad
import Data.List
import qualified Data.Set as Set

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Solution Integer
                deriving Eq

instance Show Solution where
    show (Solution c) = show (c `mod` 1000000007)

data Problem = Problem [String] deriving (Eq,Show)

parse [] = []
parse (_:cars:rest) = Problem (words cars) : parse rest

solve (Problem cars)
    | length cars == 1                                                    = Solution 1
    | any (\c -> length [m | m <- carMiddles, Set.member c m] > 1) ['a'..'z'] = Solution 0
    | otherwise        = Solution $ fromIntegral $ length [combo | combo <- allCombos $ map reducedCar cars]
    where carMiddles = [m | m <- map (Set.fromList . carMiddle . reducedCar) cars, not (Set.null m)]


carMiddle :: String -> String
carMiddle [] = []
carMiddle (_:[]) = []
carMiddle (_:_:[]) = []
carMiddle xs = tail.init $ xs

reducedCar :: String -> String
reducedCar car = map head (group car)

allCombos :: [String] -> [String]
allCombos xs = concatMap allCombos' [x:(xs\\[x]) | x <- xs]

allCombos' :: [String] -> [String]
allCombos' (x:[]) = [x]
allCombos' (x:ys) = concatMap allCombos' [(x++y):(ys\\[y]) | y <- ys, validCombo (x++y)]

validCombo :: String -> Bool
validCombo xs = length unique == Set.size set
   where unique = map head $ group xs
         set = Set.fromList unique
