{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
-- import {-@ HTF_TESTS @-} Y2015.R1A.ATest
-- import {-@ HTF_TESTS @-} Y2015.R1A.BTest
import {-@ HTF_TESTS @-} Y2015.R1A.CTest

main :: IO ()
main = htfMain htf_importedTests
