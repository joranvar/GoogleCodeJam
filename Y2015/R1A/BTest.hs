{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2015.R1A.BTest where

import Control.Parallel.Strategies
import Test.Framework

import Y2015.R1A.B

runsolve = unlines . showSolutions . parMap rdeepseq (show . solve) . parse . tail . lines
    where showSolutions = zipWith (++) ["Case #" ++ show i ++ ": " | i <- [1::Int ..]]

test_example :: IO ()
test_example = assertEqual example_solution $ runsolve example

example = "3\n\
\2 4\n\
\10 5\n\
\3 12\n\
\7 7 7\n\
\3 8\n\
\4 2 1"
example_solution = "Case #1: 1\n\
                    \Case #2: 3\n\
                    \Case #3: 1\n"

-- test_case0 :: IO ()
-- test_case0 = assertEqual (Solution False) $ solve (Problem 1 "")
test_case_min_min :: IO ()
test_case_min_min = assertEqual (Solution 1) $ solve (Problem 1 1 [1])
test_case_min_min1 ::IO ()
test_case_min_min1 = assertEqual (Solution 1) $ solve (Problem 1 999 [1])
test_case_max_min :: IO ()
test_case_max_min = assertEqual (Solution 5) $ solve (Problem 5 1000000000 [25,25,25,25,25])
test_case_max_min1 :: IO ()
test_case_max_min1 = assertEqual (Solution 2) $ solve (Problem 5 2 [25,25,25,25,24])
test_case_max_min3 :: IO ()
test_case_max_min3 = assertEqual (Solution 3) $ solve (Problem 5 3 [25,25,25,25,24])
test_case_max_min2 :: IO ()
test_case_max_min2 = assertEqual (Solution 4) $ solve (Problem 5 4 [25,25,25,25,24])
test_case_max_min6 :: IO ()
test_case_max_min6 = assertEqual (Solution 5) $ solve (Problem 5 5 [25,25,25,25,24])
test_case_max_min5 :: IO ()
test_case_max_min5 = assertEqual (Solution 4) $ solve (Problem 5 6 [22,24,29,21,24])
                   -- test_case_max_max :: IO ()
-- test_case_max_max = assertEqual (Solution 0 0) $ solve (Problem (replicate 1000 10000))

{-# ANN module ("hlint:ignore Use camelCase"::String) #-}
