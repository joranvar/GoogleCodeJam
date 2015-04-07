module Y2010.R1A.A where

import Data.List
import Data.Maybe

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Neither | Both | Red | Blue
                deriving (Eq, Show)

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

solve (Problem _board _k) = Neither
