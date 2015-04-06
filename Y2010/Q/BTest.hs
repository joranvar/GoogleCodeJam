{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2010.Q.BTest where

import Test.Framework
import Y2010.Q.B

test_parse :: IO ()
test_parse = assertEqual (Problem [1,10,11])
             $ head $ parse
             ["3 1 10 11"]

example0 = "3 26000000 11000000 6000000"
example1 = "3 1 10 11"
example2 = "2 800000000000000000001 900000000000000000001"

test_example0 :: IO ()
test_example0 = assertEqual (Solution 4000000) $ solve . head . parse . lines $ example0
test_example1 :: IO ()
test_example1 = assertEqual (Solution 0) $ solve . head . parse . lines $ example1
test_example2 :: IO ()
test_example2 = assertEqual (Solution 99999999999999999999) $ solve . head . parse . lines $ example2

{-# ANN module ("hlint:ignore Use camelCase"::String) #-}
