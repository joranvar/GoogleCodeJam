{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2015.Q.CTest where

import Control.Parallel.Strategies
import Test.Framework

import Y2015.Q.C

runsolve = unlines . showSolutions . map (show . solve) . parse . tail . lines
    where showSolutions = zipWith (++) ["Case #" ++ show i ++ ": " | i <- [1::Int ..]]

test_example :: IO ()
test_example = assertEqual example_solution $ runsolve example

example = "5\n\
\2 1\n\
\ik\n\
\3 1\n\
\ijk\n\
\3 1\n\
\kji\n\
\2 6\n\
\ji\n\
\1 10000\n\
\i"
example_solution = "Case #1: NO\n\
\Case #2: YES\n\
\Case #3: NO\n\
\Case #4: YES\n\
\Case #5: NO\n"

test_case0 :: IO ()
test_case0 = assertEqual (Solution False) $ solve (Problem 1 "")
test_case1 :: IO ()
test_case1 = assertEqual (Solution True) $ solve (Problem 1 "ijk")
test_case2 :: IO ()
test_case2 = assertEqual (Solution True) $ solve (Problem 6 "ji")
test_caseNot :: IO ()
test_caseNot = assertEqual (Solution False) $ solve (Problem 1 "ijkk")
test_caseNot2 :: IO ()
test_caseNot2 = assertEqual (Solution False) $ solve (Problem 2 "ijk")
test_caseInvert :: IO ()
test_caseInvert = assertEqual (Solution True) $ solve (Problem 1 "iiiiijk")

test_advance :: IO ()
test_advance = assertEqual QuaternionState {negative=False, stack=Nothing} $
               foldl advance (QuaternionState False Nothing) $ replicate 4 'i'
test_advanceHalf :: IO ()
test_advanceHalf = assertEqual QuaternionState {negative=True, stack=Nothing} $
                   foldl advance (QuaternionState False Nothing) $ replicate 2 'i'
test_advanceMinusKPlusK :: IO ()
test_advanceMinusKPlusK = assertEqual QuaternionState {negative=False, stack=Nothing} $
                          foldl advance (QuaternionState False Nothing) "jiij"

-- test_case0 :: IO ()
-- test_case0 = assertEqual (Solution 1) $ solve (Problem [1])
-- test_case1 :: IO ()
-- test_case1 = assertEqual (Solution 6) $ solve (Problem [3,8,3,8])
-- test_case_max_small :: IO ()
-- test_case_max_small = assertEqual (Solution 9) $ solve (Problem $ replicate 6 9)
-- test_case_max_large :: IO ()
-- test_case_max_large = assertEqual (Solution 1000) $ solve (Problem $ replicate 1000 1000)
-- test_case_min_large :: IO ()
-- test_case_min_large = assertEqual (Solution 94) $ solve (Problem $ replicate 2 1000)

{-# ANN module ("hlint:ignore Use camelCase"::String) #-}
