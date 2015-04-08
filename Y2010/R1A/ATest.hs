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

example0 = "7 3\n\
\.......\n\
\.......\n\
\.......\n\
\...R...\n\
\...BB..\n\
\..BRB..\n\
\.RRBR.."

example1 = "6 4\n\
\......\n\
\......\n\
\.R...R\n\
\.R..BB\n\
\.R.RBR\n\
\RB.BBB"

example2 = "4 4\n\
\R...\n\
\BR..\n\
\BR..\n\
\BR.."

example3 = "3 3\n\
\B..\n\
\RB.\n\
\RB."

test_example0 :: IO ()
test_example0 = assertEqual (Solution False False) $ solve . head . parse . lines $ example0
test_example1 :: IO ()
test_example1 = assertEqual (Solution True True) $ solve . head . parse . lines $ example1
test_example2 :: IO ()
test_example2 = assertEqual (Solution True False) $ solve . head . parse . lines $ example2
test_example3 :: IO ()
test_example3 = assertEqual (Solution False True) $ solve . head . parse . lines $ example3

{-# ANN module ("hlint:ignore Use camelCase"::String) #-}
