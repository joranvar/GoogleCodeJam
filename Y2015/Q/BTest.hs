{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2015.Q.BTest where

import Control.Parallel.Strategies
import Test.Framework

import Y2015.Q.B

runsolve = unlines . showSolutions . parMap rdeepseq (show . solve) . parse . tail . lines
    where showSolutions = zipWith (++) ["Case #" ++ show i ++ ": " | i <- [1::Int ..]]

test_example :: IO ()
test_example = assertEqual example_solution $ runsolve example

example = "3\n\
\1\n\
\3\n\
\4\n\
\1 2 1 2\n\
\1\n\
\4"
example_solution = "Case #1: 3\n\
\Case #2: 2\n\
\Case #3: 3\n"

test_case0 :: IO ()
test_case0 = assertEqual (Solution 1) $ solve (Problem [1])
test_case1 :: IO ()
test_case1 = assertEqual (Solution 6) $ solve (Problem [3,8,3,8])
test_case2 :: IO ()
test_case2 = assertEqual (Solution 5) $ solve (Problem [3,3,3,9])
test_case3 :: IO ()
test_case3 = assertEqual (Solution 99) $ solve (Problem [1..99])
test_case_max_small :: IO ()
test_case_max_small = assertEqual (Solution 9) $ solve (Problem $ replicate 6 9)
test_case_max_small2 :: IO ()
test_case_max_small2 = assertEqual (Solution 9) $ solve (Problem $ replicate 6 8 ++ [9])
test_case_max_small3 :: IO ()
test_case_max_small3 = assertEqual (Solution 5) $ solve (Problem [9,3])
test_case_max_small4 :: IO ()
test_case_max_small4 = assertEqual (Solution 6) $ solve (Problem [9,5])
test_case_max_small5 :: IO ()
test_case_max_small5 = assertEqual (Solution 7) $ solve (Problem [9,5,9])
test_case_max_small6 :: IO ()
test_case_max_small6 = assertEqual (Solution 8) $ solve (Problem [9,5,9,9])
test_case_max_large :: IO ()
test_case_max_large = assertEqual (Solution 1000) $ solve (Problem $ replicate 1000 1000)
test_case_min_large :: IO ()
test_case_min_large = assertEqual (Solution 88) $ solve (Problem $ replicate 2 1000)

{-# ANN module ("hlint:ignore Use camelCase"::String) #-}
