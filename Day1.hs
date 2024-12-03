module Day1 where

import Data.Char
import Data.List
import Data.List.Split (splitOn)
import Shared

part = genDay part1 part2

pairUp :: [b] -> (b, b)
pairUp [x, y] = (x, y)

sortPair :: (Ord a, Ord b) => ([a], [b]) -> ([a], [b])
sortPair (list1, list2) = (sort list1, sort list2)

parseInput :: String -> ([Int], [Int])
parseInput input = unzip . map (pairUp . map asInt . splitOn "   ") $ lines input

part1 :: String -> Int
part1 input = let (x, y) = sortPair $ parseInput input in sum $ zipWith (-) x y

sumSimilarity :: [Int] -> [Int] -> Int -> Int
sumSimilarity (x : xs) second buf = buf + sumSimilarity xs second (x * length (filter (== x) second))
sumSimilarity [] _ _ = 0

part2 input =
  let (first, second) = parseInput input in sumSimilarity first second 0