{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Y2014.R1C.BTest

main :: IO ()
main = htfMain htf_importedTests

