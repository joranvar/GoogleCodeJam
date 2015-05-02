{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2015.R1A.ATest where

import Control.Parallel.Strategies
import Test.Framework

import Y2015.R1A.A

runsolve = unlines . showSolutions . parMap rdeepseq (show . solve) . parse . tail . lines
    where showSolutions = zipWith (++) ["Case #" ++ show i ++ ": " | i <- [1::Int ..]]

test_example :: IO ()
test_example = assertEqual example_solution $ runsolve example

example = "4\n\
\4\n\
\10 5 15 5\n\
\2\n\
\100 100\n\
\8\n\
\81 81 81 81 81 81 81 0\n\
\6\n\
\23 90 40 0 100 9"
example_solution = "Case #1: 15 25\n\
\Case #2: 0 0\n\
\Case #3: 81 567\n\
\Case #4: 181 244\n"

-- test_case0 :: IO ()
-- test_case0 = assertEqual (Solution False) $ solve (Problem 1 "")
test_case_max_min :: IO ()
test_case_max_min = assertEqual (Solution 0 0) $ solve (Problem (replicate 10 100))
test_case_max_max :: IO ()
test_case_max_max = assertEqual (Solution 0 0) $ solve (Problem (replicate 1000 10000))
{-# ANN module ("hlint:ignore Use camelCase"::String) #-}
