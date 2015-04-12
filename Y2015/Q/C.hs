module Y2015.Q.C where

import Data.List
import Data.Maybe
import Data.String.Utils

solve :: Problem -> Solution
parse :: [String] -> [Problem]

data Solution = Solution Bool
              deriving (Eq)
instance Show Solution
    where show (Solution True) = "YES"
          show (Solution False) = "NO"

data Problem = Problem { x :: Int
                       , s :: String
                       } deriving (Eq, Show)

data Quaternion = I
                | J
                | K
                  deriving (Eq, Show)

parse (lx:s:rest) =
    let (_:x:[]) = map read $ words lx in
    Problem x s : parse rest
parse [] = []

solve (Problem x s)
    | length s * x < 3 = Solution False
    | length (group s) == 1 = Solution False
    | foldl advance (QuaternionState False Nothing)
      (concat $ replicate effectiveLength (strip s)) /= QuaternionState True Nothing = Solution False
    | otherwise = Solution ((length s * effectiveLength) `elem` isIjkable (concat $ replicate effectiveLength (strip s)))
    where isIjkable s =
            let hasI = last $ parseIJK I s (Just 0)
                hasJ = listToMaybe $ parseIJK J s (Just hasI)
            in parseIJK K s hasJ
          effectiveLength
                | x >= 4 = x `mod` 4 + 4
                | otherwise = x

parseIJK :: Quaternion -> String -> Maybe Int -> [Int]
parseIJK q s i
    | isNothing i = []
    | length s <= fromMaybe 0 i = []
    | otherwise = snd $ foldl getMatchingIndices (QuaternionState False Nothing, []) (zip [j..] (drop j s))
    where getMatchingIndices (qState, indices) c =
              let q' = advance qState $ snd c in
              (q', if stack q' == Just q && (not . negative) q'
                   then (fst c + 1) : indices
                   else indices)
          j = fromMaybe 0 i

data QuaternionState = QuaternionState { negative :: Bool
                                       , stack :: Maybe Quaternion
                                       } deriving (Eq, Show)

advance :: QuaternionState -> Char -> QuaternionState
advance (QuaternionState {negative=n, stack=s}) c
    | isNothing s = QuaternionState n (Just $ stateFrom c)
    | s == Just I && c == 'i' = QuaternionState (not n) Nothing
    | s == Just I && c == 'j' = QuaternionState n (Just K)
    | s == Just I && c == 'k' = QuaternionState (not n) (Just J)
    | s == Just J && c == 'i' = QuaternionState (not n) (Just K)
    | s == Just J && c == 'j' = QuaternionState (not n) Nothing
    | s == Just J && c == 'k' = QuaternionState n (Just I)
    | s == Just K && c == 'i' = QuaternionState n (Just J)
    | s == Just K && c == 'j' = QuaternionState (not n) (Just I)
    | s == Just K && c == 'k' = QuaternionState (not n) Nothing
    where stateFrom 'i' = I
          stateFrom 'j' = J
          stateFrom 'k' = K
