{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
-- import {-@ HTF_TESTS @-} Y2010.Q.ATest
import {-@ HTF_TESTS @-} Y2010.Q.BTest
-- import {-@ HTF_TESTS @-} Y2014.Q.ATest
-- import {-@ HTF_TESTS @-} Y2014.R1A.ATest
-- import {-@ HTF_TESTS @-} Y2014.R1C.ATest
-- import {-@ HTF_TESTS @-} Y2014.R1C.BTest

main :: IO ()
main = htfMain htf_importedTests
