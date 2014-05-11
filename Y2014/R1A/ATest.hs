{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2014.R1A.ATest where

import Test.Framework
import Y2014.R1A.A

simpleInput = "3 2\n\
\01 11 10\n\
\11 00 10\n\
\2 3\n\
\101 111\n\
\010 001\n\
\2 2\n\
\01 10\n\
\10 01"

test_parse :: IO ()
test_parse = assertEqual [Problem [1,3,2] [3,0,2]
                         ,Problem [5,7] [2,1]
                         ,Problem [1,2] [2,1]] $ parse $ lines simpleInput

test_sample1 :: IO ()
test_sample1 = assertEqual (Switches 1) $ solve $ Problem [1,3,2] [3,0,2]

test_sample2 :: IO ()
test_sample2 = assertEqual True True

test_sample3 :: IO ()
test_sample3 = assertEqual True True
