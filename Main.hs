module Main where

import Y2010.Q.B (solve, parse)

main :: IO ()
main = getContents >>=
       putStr . unlines . showSolutions . map (show . solve) . parse . tail . lines

showSolutions :: [String] -> [String]
showSolutions = zipWith (++) ["Case #" ++ show i ++ ": " | i <- [1::Int ..]]
