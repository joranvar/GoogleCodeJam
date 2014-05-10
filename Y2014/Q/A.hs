module Y2014.Q.A where

import qualified Data.Set as Set

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = BadMagician
              | VolunteerCheated
              | Card Int
                deriving Eq

instance Show Solution where
    show BadMagician      = "Bad magician!"
    show VolunteerCheated = "Volunteer cheated!"
    show (Card c)         = show c

data Problem = Problem { first :: Int
                       , firstCards :: [[Int]]
                       , second :: Int
                       , secondCards :: [[Int]]
                       } deriving (Eq,Show)

parse (f:c11:c12:c13:c14:s:c21:c22:c23:c24:rest) =
    Problem
    (read f)
    [map read $ words c | c <- [c11,c12,c13,c14]]
    (read s)
    [map read $ words c | c <- [c21,c22,c23,c24]]
    : parse rest
parse [] = []

solve (Problem f fc s sc)
    | Set.size overlap > 1 = BadMagician
    | Set.size overlap == 0 = VolunteerCheated
    | otherwise = Card (head (Set.elems overlap))
    where overlap = Set.fromList (fc !! (f-1)) `Set.intersection` Set.fromList (sc !! (s-1))
