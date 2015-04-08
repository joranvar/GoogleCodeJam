module Y2010.R1A.A where

import Data.List
import Data.Maybe

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Solution { red :: Bool
                         , blue :: Bool
                         } deriving (Eq, Show)

data Piece = R | B | N
           deriving (Eq, Show)

data Problem = Problem { board :: [[Piece]]
                       , k :: Int
                       } deriving (Eq, Show)

parse (dimensions:rest) =
    let [n:_k:_] = [map read $ words dimensions]
    in Problem (map (map readPiece) $ take n rest) _k : parse rest
    where readPiece '.' = N
          readPiece 'R' = R
          readPiece 'B' = B
parse [] = []

solve (Problem _board _k)
    | count B < _k && count R < _k = Solution False False
    | count B < _k = Solution (checkWin R) False
    | count R < _k = Solution False (checkWin B)
    | otherwise = Solution (checkWin R) (checkWin B)
    where count x = sum $ map (\i -> if i == x then 1 else 0) $ concat _board
          shrunkBoard = dropWhile (all (== N)) _board
          tiltedBoard = map (reverse . filter (not . (==) N)) shrunkBoard
          checkWin x = horizCheckWin x tiltedBoard || vertCheckWin x tiltedBoard || diagACheckWin x tiltedBoard || diagBCheckWin x tiltedBoard
          horizCheckWin x = any (horizWin x)
          horizWin x line = any (\l -> length l >= _k && head l == x) $ group line
          vertCheckWin x b = horizCheckWin x $ transpose b
          diagACheckWin x b = False
          diagBCheckWin x b = diagBCheckWin x $ transpose b
