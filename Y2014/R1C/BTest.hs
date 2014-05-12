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

test_singleCar_Solution1 =
    assertEqual (Solution 1) $ solve $ Problem ["abc"]

test_singlePureCar_Solution1 =
    assertEqual (Solution 1) $ solve $ Problem ["aaa"]

test_doublePureCar_Solution2 =
    assertEqual (Solution 2) $ solve $ Problem ["aaa", "a"]

test_doubleCar_Solution2 =
    assertEqual (Solution 2) $ solve $ Problem ["aaa", "bcd"]

test_triplePureCar_Solution6 =
    assertEqual (Solution 6) $ solve $ Problem ["aaa", "aaa", "aaa"]

test_threePureCarOneDifferent_Solution4 =
    assertEqual (Solution 4) $ solve $ Problem ["aaa", "a", "bbbb"]

test_doubleDoublePureCar_Solution8 =
    assertEqual (Solution 8) $ solve $ Problem ["aa","aaaa","vv","vvvvv"]

test_doubleDoublePureCarPlusBinder_Solution4 =
    assertEqual (Solution 4) $ solve $ Problem ["aa","aaaa","vv","vvvvv", "av"]

test_doubleDoublePureCarPlusPrefixBinder_Solution8 =
    assertEqual (Solution 8) $ solve $ Problem ["aa","aaaa","vv","vvvvv", "ag"]

test_twoIdenticallyBoundCars_Solution0 =
    assertEqual (Solution 0) $ solve $ Problem ["ab","ab"]

test_singleInvalidCar_Solution0 =
    assertEqual (Solution 0) $ solve $ Problem ["cac"]
