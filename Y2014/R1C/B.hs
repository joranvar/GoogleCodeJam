module Y2014.R1C.B where

import Data.List
import Data.List.Utils
import Data.Maybe
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

-- Cars with the same letter have to be combined each other: less mobile
-- For any letter, there can only be two cars with also another letter
-- Cars with unique letters can be put anywhere left

solve (Problem c)
    -- Any single offending car is invalid
    | any (not . validCombo) cars = Solution 0
    -- Any two equal cars with more than one letter (-kind) is invalid; needed for sets
    | any (\grp -> length grp > 1) $ group [car | car <- cars, length car > 1] = Solution 0
    -- Otherwise, count pure equal cars (one letter) as permutations and permutate the set
    | otherwise = let pureCarsPossibilitiesCount = product $ map (factorial . length) ([]:group pureCars)
                      factorial l                = product (take (l+1) (1:[1..]))
                      pureCars                   = [car | car <- cars, length car == 1]
                      set                        = uniq cars
                  in case allCombos set of
                       Nothing -> Solution 0
                       Just xs -> Solution (pureCarsPossibilitiesCount * fromIntegral (length xs))
    where cars = sort $ map reducedCar c

reducedCar     :: String -> String
reducedCar car = map head (group car)

allCombos    :: [String] -> Maybe [String]
allCombos xs = case mapM allCombos' [x:(xs\\[x]) | x <- xs] of
                 Nothing -> Nothing
                 Just a  -> Just $ concat a

allCombos'         :: [String] -> Maybe [String]
allCombos' (x:[])
    | validCombo x = Just [x]
    | otherwise    = Nothing
allCombos' (x:ys)  = case map allCombos' [(x++y):(ys\\[y]) | y <- ys, validCombo (x++y)] of
                       xs
                           | any isNothing xs -> Nothing
                           | otherwise        -> case sequence xs of
                                                   Nothing  -> Nothing
                                                   Just mxs -> Just (concat mxs)

validCombo    :: String -> Bool
validCombo xs = length unique == Set.size set
   where unique = map head $ group xs
         set    = Set.fromList unique
