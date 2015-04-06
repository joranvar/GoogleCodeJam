module Y2010.Q.C where

import Data.List
import Data.Maybe

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Solution Int
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
    where total = sumRides _rides
          sumRides cnt
              | cnt <= length allGroups = sum $ take cnt allGroups
              | otherwise =
                  sum (take cnt allGroups)
                  + (((cnt - length allGroups) `div` length repeatingGroups) * sum repeatingGroups)
                  + sum (take ((cnt - length allGroups) `mod` length repeatingGroups) repeatingGroups)
          repeatingGroups = drop repeatingIndex allGroups
          repeatingIndex = snd findGroups
          allGroups = fst findGroups
          findGroups = groups [] 0
          groups pastGroups ind
              | ind `elem` pastGroups = ([], fromMaybe 0 (elemIndex ind $ reverse pastGroups) `mod` length _queue)
              | otherwise = (fst (cachedGroup ind) : fst (groups (ind:pastGroups) (snd (cachedGroup ind))), snd $ groups (ind:pastGroups) (snd (cachedGroup ind)))
          realQueue = cycle _queue
          cachedGroup ind = map group [0..(length _queue)] !! (ind `mod` length _queue)
          group ind =
              let group' cur ind
                      | cur == sum _queue = (cur, ind)
                      | cur + realQueue!!ind <= _capacity = group' (cur + realQueue!!ind) (ind+1)
                      | otherwise = (cur, ind)
              in group' 0 ind
