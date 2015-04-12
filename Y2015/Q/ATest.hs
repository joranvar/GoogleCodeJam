{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2015.Q.ATest where

import Control.Parallel.Strategies
import Test.Framework

import Y2015.Q.A

runsolve = unlines . showSolutions . parMap rdeepseq (show . solve) . parse . tail . lines
    where showSolutions = zipWith (++) ["Case #" ++ show i ++ ": " | i <- [1::Int ..]]

test_example :: IO ()
test_example = assertEqual example_solution $ runsolve example

example = "4\n\
\4 11111\n\
\1 09\n\
\5 110011\n\
\0 1"
example_solution = "Case #1: 0\n\
\Case #2: 1\n\
\Case #3: 2\n\
\Case #4: 0\n"

test_case0 :: IO ()
test_case0 = assertEqual (Solution 0) $ solve (Problem [1])
test_case1 :: IO ()
test_case1 = assertEqual (Solution 1) $ solve (Problem [0,1])
test_case2 :: IO ()
test_case2 = assertEqual (Solution 1) $ solve (Problem [1,0,2])
test_case3 :: IO ()
test_case3 = assertEqual (Solution 2) $ solve (Problem [1,0,0,2])
test_case4 :: IO ()
test_case4 = assertEqual (Solution 3) $ solve (Problem [0,0,0,2])
test_case5 :: IO ()
test_case5 = assertEqual (Solution 2) $ solve (Problem [0,0,3,0,3,0,1])

{-# ANN module ("hlint:ignore Use camelCase"::String) #-}
