{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2010.Q.ATest where

import Test.Framework
import Y2010.Q.A

test_parse :: IO ()
test_parse = assertEqual (Problem 1 0)
             $ head $ parse
             ["1 0"]

example1  = "1 0"
example2  = "1 1"
example3  = "4 0"
example4  = "4 47"

test_example1_returnsOff :: IO ()
test_example1_returnsOff = assertEqual OFF $ solve . head . parse . lines $ example1
test_example2_returnsOn :: IO ()
test_example2_returnsOn = assertEqual ON $ solve . head . parse . lines $ example2
test_example3_returnsOff :: IO ()
test_example3_returnsOff = assertEqual OFF $ solve . head . parse . lines $ example3
test_example4_returnsOn :: IO ()
test_example4_returnsOn = assertEqual ON $ solve . head . parse . lines $ example4

{-# ANN module ("hlint:ignore Use camelCase"::String) #-}
