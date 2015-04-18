module Main where

import Control.Parallel.Strategies
-- import Y2015.Q.A (solve, parse)
-- import Y2015.Q.B (solve, parse)
-- import Y2015.Q.C (solve, parse)
import Y2015.Q.D (solve, parse)

main :: IO ()
main = getContents >>=
       putStr . unlines . showSolutions . parMap rdeepseq (show . solve) . parse . tail . lines

showSolutions :: [String] -> [String]
showSolutions = zipWith (++) ["Case #" ++ show i ++ ": " | i <- [1::Int ..]]
