{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2015.Q.DTest where

import Control.Parallel.Strategies
import Test.Framework

import Y2015.Q.D

runsolve = unlines . showSolutions . map (show . solve) . parse . tail . lines
    where showSolutions = zipWith (++) ["Case #" ++ show i ++ ": " | i <- [1::Int ..]]

test_example :: IO ()
test_example = assertEqual example_solution $ runsolve example
test_small :: IO ()
test_small = assertEqual small_solution $ runsolve small

example = "4\n\
\2 2 2\n\
\2 1 3\n\
\4 4 1\n\
\3 2 3"
example_solution = "Case #1: GABRIEL\n\
\Case #2: RICHARD\n\
\Case #3: RICHARD\n\
\Case #4: GABRIEL\n"

small="64\n\
\2 2 2\n\
\2 1 3\n\
\4 4 1\n\
\3 2 3\n\
\3 4 4\n\
\3 3 2\n\
\4 3 2\n\
\3 3 1\n\
\1 1 4\n\
\1 4 1\n\
\4 1 4\n\
\1 3 4\n\
\2 3 3\n\
\2 3 1\n\
\1 1 1\n\
\1 2 4\n\
\1 4 4\n\
\2 2 4\n\
\2 4 2\n\
\1 4 2\n\
\4 4 2\n\
\2 3 4\n\
\2 1 1\n\
\1 1 2\n\
\4 1 3\n\
\1 1 3\n\
\2 4 4\n\
\3 4 1\n\
\1 2 1\n\
\2 2 1\n\
\4 1 2\n\
\2 3 2\n\
\4 2 1\n\
\4 2 2\n\
\2 1 4\n\
\4 2 4\n\
\3 1 4\n\
\3 4 2\n\
\3 1 3\n\
\3 3 3\n\
\4 3 3\n\
\2 1 2\n\
\4 3 1\n\
\4 4 4\n\
\1 3 3\n\
\3 4 3\n\
\1 3 2\n\
\1 3 1\n\
\3 1 2\n\
\2 4 1\n\
\2 2 3\n\
\2 4 3\n\
\3 2 2\n\
\3 2 1\n\
\3 3 4\n\
\4 2 3\n\
\1 4 3\n\
\3 1 1\n\
\3 2 4\n\
\4 1 1\n\
\4 3 4\n\
\4 4 3\n\
\1 2 3\n\
\1 2 2"
small_solution="Case #1: GABRIEL\n\
\Case #2: RICHARD\n\
\Case #3: RICHARD\n\
\Case #4: GABRIEL\n\
\Case #5: RICHARD\n\
\Case #6: GABRIEL\n\
\Case #7: RICHARD\n\
\Case #8: RICHARD\n\
\Case #9: GABRIEL\n\
\Case #10: GABRIEL\n\
\Case #11: RICHARD\n\
\Case #12: GABRIEL\n\
\Case #13: RICHARD\n\
\Case #14: RICHARD\n\
\Case #15: GABRIEL\n\
\Case #16: GABRIEL\n\
\Case #17: GABRIEL\n\
\Case #18: GABRIEL\n\
\Case #19: GABRIEL\n\
\Case #20: GABRIEL\n\
\Case #21: RICHARD\n\
\Case #22: GABRIEL\n\
\Case #23: RICHARD\n\
\Case #24: GABRIEL\n\
\Case #25: RICHARD\n\
\Case #26: GABRIEL\n\
\Case #27: GABRIEL\n\
\Case #28: RICHARD\n\
\Case #29: GABRIEL\n\
\Case #30: GABRIEL\n\
\Case #31: RICHARD\n\
\Case #32: GABRIEL\n\
\Case #33: RICHARD\n\
\Case #34: RICHARD\n\
\Case #35: GABRIEL\n\
\Case #36: RICHARD\n\
\Case #37: RICHARD\n\
\Case #38: RICHARD\n\
\Case #39: RICHARD\n\
\Case #40: GABRIEL\n\
\Case #41: RICHARD\n\
\Case #42: GABRIEL\n\
\Case #43: RICHARD\n\
\Case #44: GABRIEL\n\
\Case #45: GABRIEL\n\
\Case #46: GABRIEL\n\
\Case #47: GABRIEL\n\
\Case #48: GABRIEL\n\
\Case #49: RICHARD\n\
\Case #50: GABRIEL\n\
\Case #51: GABRIEL\n\
\Case #52: GABRIEL\n\
\Case #53: RICHARD\n\
\Case #54: RICHARD\n\
\Case #55: GABRIEL\n\
\Case #56: RICHARD\n\
\Case #57: GABRIEL\n\
\Case #58: RICHARD\n\
\Case #59: RICHARD\n\
\Case #60: RICHARD\n\
\Case #61: GABRIEL\n\
\Case #62: GABRIEL\n\
\Case #63: GABRIEL\n\
\Case #64: GABRIEL\n"

test_case0 :: IO ()
test_case0 = assertEqual (Solution False) $ solve (Problem 5 2 99)
test_case1 :: IO ()
test_case1 = assertEqual (Solution True) $ solve (Problem 5 4 5)
-- test_case2 :: IO ()
-- test_case2 = assertEqual (Solution True) $ solve (Problem 6 "ji")
-- test_caseNot :: IO ()
-- test_caseNot = assertEqual (Solution False) $ solve (Problem 1 "ijkk")
-- test_caseNot2 :: IO ()
-- test_caseNot2 = assertEqual (Solution False) $ solve (Problem 2 "ijk")
-- test_caseInvert :: IO ()
-- test_caseInvert = assertEqual (Solution True) $ solve (Problem 1 "iiiiijk")

-- test_advance :: IO ()
-- test_advance = assertEqual QuaternionState {negative=False, stack=Nothing} $
--                foldl advance (QuaternionState False Nothing) $ replicate 4 'i'
-- test_advanceHalf :: IO ()
-- test_advanceHalf = assertEqual QuaternionState {negative=True, stack=Nothing} $
--                    foldl advance (QuaternionState False Nothing) $ replicate 2 'i'
-- test_advanceMinusKPlusK :: IO ()
-- test_advanceMinusKPlusK = assertEqual QuaternionState {negative=False, stack=Nothing} $
--                           foldl advance (QuaternionState False Nothing) "jiij"

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
