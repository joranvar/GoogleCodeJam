{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
-- import {-@ HTF_TESTS @-} Y2015.Q.ATest
-- import {-@ HTF_TESTS @-} Y2015.Q.BTest
-- import {-@ HTF_TESTS @-} Y2015.Q.CTest
import {-@ HTF_TESTS @-} Y2015.Q.DTest

main :: IO ()
main = htfMain htf_importedTests
