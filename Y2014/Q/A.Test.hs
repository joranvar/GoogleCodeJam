{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2014.Q.ATest where

import Test.Framework
import Y2014.Q.A

test_ZeroIsZero :: IO ()
test_ZeroIsZero = assertEqual BadMagician $ solve Problem 1 [[1]] 1 [[1]]
