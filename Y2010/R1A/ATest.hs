{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Y2010.R1A.ATest where

import Test.Framework
import Y2010.R1A.A

test_parse :: IO ()
test_parse = assertEqual (Problem [[R,N,N,N],
                                   [B,R,N,N],
                                   [B,R,N,N],
                                   [B,R,N,N]] 4)
             $ head $ parse
             ["4 4", "R...", "BR..", "BR..", "BR.."]

-- example0 = "4 6 4\n\
-- \1 4 2 1"
-- example1 = "100 10 1\n\
-- \1"
-- example2 = "5 5 10\n\
-- \2 4 2 3 4 2 1 2 1 3"

-- test_example0 :: IO ()
-- test_example0 = assertEqual (Solution 21) $ solve . head . parse . lines $ example0
-- test_example1 :: IO ()
-- test_example1 = assertEqual (Solution 100) $ solve . head . parse . lines $ example1
-- test_example2 :: IO ()
-- test_example2 = assertEqual (Solution 20) $ solve . head . parse . lines $ example2

{-# ANN module ("hlint:ignore Use camelCase"::String) #-}
