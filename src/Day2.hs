module Day2 (part) where

import Data.List.Split (splitOn)
import Shared (AsInt (asInt), genDay, isSorted, remByIdx)

part :: String -> String -> Int
part = genDay part1 part2

parseInput :: String -> [[Int]]
parseInput input = map (map asInt . splitOn " ") (lines input)

diffSafe :: (Ord a, Num a) => a -> a -> Bool
diffSafe x y = let dist = abs (x - y) in dist >= 1 && dist <= 3

thing :: [Int] -> Bool -> Bool
thing (x : xs) bool = bool && thing xs (diffSafe x (head xs))
thing [] _ = True

sortedEither :: Ord a => [a] -> Bool
sortedEither list = isSorted list || isSorted (reverse list)

createVers :: [Int] -> Int -> [[Int]] -> [[Int]]
createVers xs idx buf
  | idx < length xs = createVers xs (idx + 1) (remByIdx xs idx : buf)
  | otherwise = buf

perms :: [Int] -> [[Int]]
perms xs = createVers xs 0 []

dampenedIsSafe :: [Int] -> Bool
dampenedIsSafe list = isSafe list || any isSafe (perms list)

isSafe :: [Int] -> Bool
isSafe list = thing list True && sortedEither list

sumSafe :: ([Int] -> Bool) -> String -> Int
sumSafe safeFunc input = sum $ map (asInt . safeFunc) (parseInput input)

part1 :: String -> Int
part1 = sumSafe isSafe

part2 :: String -> Int
part2 = sumSafe dampenedIsSafe
