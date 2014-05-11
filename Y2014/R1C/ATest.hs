{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2014.R1C.ATest where

import Data.Ratio
import Test.Framework
import Y2014.R1C.A

simpleInput = "1/2\n\
\3/4\n\
\1/4\n\
\2/23\n\
\123/31488"

test_parse :: IO ()
test_parse = assertEqual [Problem (1 % 2)
                         ,Problem (3 % 4)
                         ,Problem (1 % 4)
                         ,Problem (2 % 23)
                         ,Problem (123 % 31488)] $ parse $ lines simpleInput

test_solve1 = assertEqual (Generations 1) $ solve $ Problem (1%2)

test_solve2 = assertEqual (Generations 1) $ solve $ Problem (3%4)

test_solve3 = assertEqual NotPossible $ solve $ Problem (2%23)

test_solve4 = assertEqual (Generations 2) $ solve (Problem (1%4))

test_solve5 = assertEqual (Generations 8) $ solve $ Problem (123%31488)
