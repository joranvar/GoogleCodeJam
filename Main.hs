module Main where

import Y2014.Q.A (solve, parse)

main :: IO ()
main = getContents >>=
       putStr . unlines . showSolutions . map (show . solve) . parse . tail . lines

showSolutions :: [String] -> [String]
showSolutions = zipWith (++) ["Case #" ++ show i ++ ": " | i <- [1::Int ..]] 
