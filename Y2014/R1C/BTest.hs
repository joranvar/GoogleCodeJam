{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2014.R1C.BTest where

import Data.Ratio
import Test.Framework
import Y2014.R1C.B

simpleInput = "3\n\
\ab bbbc cd\n\
\4\n\
\aa aa bc c\n\
\2\n\
\abc bcd"

test_parse :: IO ()
test_parse = assertEqual [Problem ["ab","bbbc","cd"]
                         ,Problem ["aa","aa","bc","c"]
                         ,Problem ["abc","bcd"]
                         ]
                         $ parse $ lines simpleInput

test_solve1 :: IO ()
test_solve1 = assertEqual (Solution 1) $ solve $ Problem ["ab","bbbc","cd"]

test_solve2 :: IO ()
test_solve2 = assertEqual (Solution 4) $ solve $ Problem ["aa","aa","bc","c"]

test_solve3 :: IO ()
test_solve3 = assertEqual (Solution 0) $ solve $ Problem ["abc","bcd"]
