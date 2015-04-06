{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2014.Q.ATest where

import Test.Framework
import Y2014.Q.A

test_parse :: IO ()
test_parse = assertEqual (Problem 1 [[1,2,3,4],[2,2,3,4],[3,2,3,4],[4,2,3,4]]
                                  2 [[1,2,3,4],[2,2,3,4],[3,2,3,4],[4,2,3,4]])
             $ head $ parse
             ["1","1 2 3 4","2 2 3 4","3 2 3 4","4 2 3 4"
             ,"2","1 2 3 4","2 2 3 4","3 2 3 4","4 2 3 4"]

problem7 = "2\n\
\1 2 3 4\n\
\5 6 7 8\n\
\9 10 11 12\n\
\13 14 15 16\n\
\3\n\
\1 2 5 4\n\
\3 11 6 15\n\
\9 10 7 12\n\
\13 14 8 16"

problemBad = "2\n\
\1 2 3 4\n\
\5 6 7 8\n\
\9 10 11 12\n\
\13 14 15 16\n\
\2\n\
\1 2 3 4\n\
\5 6 7 8\n\
\9 10 11 12\n\
\13 14 15 16"

problemCheat = "2\n\
\1 2 3 4\n\
\5 6 7 8\n\
\9 10 11 12\n\
\13 14 15 16\n\
\3\n\
\1 2 3 4\n\
\5 6 7 8\n\
\9 10 11 12\n\
\13 14 15 16"

test_problem7_returnsCard7 :: IO ()
test_problem7_returnsCard7 = assertEqual (Card 7) $ solve . head . parse . lines $ problem7

test_problemBad_returnsBadMagician :: IO ()
test_problemBad_returnsBadMagician = assertEqual BadMagician $ solve . head . parse . lines $ problemBad

test_problemCheat_returnsVolunteerCheated :: IO ()
test_problemCheat_returnsVolunteerCheated = assertEqual VolunteerCheated $ solve . head . parse . lines $ problemCheat

{-# ANN module ("hlint:ignore Use camelCase"::String) #-}
