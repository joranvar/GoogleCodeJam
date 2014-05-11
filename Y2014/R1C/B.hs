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
    = Solution $ fromIntegral $ length $ allCombos $ map reducedCar cars

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
allCombos' (x:[])
    | validCombo x = [x]
    | otherwise    = []
allCombos' (x:ys) = concatMap allCombos' [(x++y):(ys\\[y]) | y <- ys, validCombo (x++y)]

validCombo :: String -> Bool
validCombo xs = length unique == Set.size set
   where unique = map head $ group xs
         set = Set.fromList unique
