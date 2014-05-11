{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Y2014.R1A.ATest

main :: IO ()
main = htfMain htf_importedTests

