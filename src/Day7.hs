module Day7 (part) where

import Shared (genDay, AsInt(asInt))
import Data.Bifunctor ( Bifunctor(bimap) )

part :: String -> String -> Int
part = genDay part1 part2

type CalibrationData = (Int, [Int])

parser :: String -> [CalibrationData]
parser = map (bimap asInt (map asInt . tail . words) . break (== ':')) . lines

isPossible :: CalibrationData -> ([Int] -> Int -> [Int]) -> Bool
isPossible (sol, terms) f = elem sol $ f terms sol

calculate :: Int -> [Int -> Int -> Int] -> [Int] -> Int -> [Int]
calculate prev funcs (x:xs) target = concatMap (\f -> if target > prev then calculate (f prev x) funcs xs target else []) funcs
calculate  value _ [] target = [value | value == target]

generalSolution :: ([Int] -> Int -> [Int]) -> String -> Int
generalSolution f xs = sum $ map (\x -> if isPossible x f then fst x else 0) $ parser xs

-- triple | cuz too lazy to do other tom foolery to make it be ||
-- essentially the `||` operator in the AoC question
(|||) :: Int -> Int -> Int
x ||| y = intCat' (0 :: Int)
  where intCat' n = if 10^n > y then y+x*10^n else intCat' (n+1) -- basically x*10^(1 + log_10 y) + y

part1 :: String -> Int
part1 = generalSolution $ calculate 0 [(+), (*)]

part2 :: String -> Int
part2 = generalSolution $ calculate 0 [(+), (*), (|||)]