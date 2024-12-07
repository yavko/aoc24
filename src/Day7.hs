module Day7 (part) where

import Shared (genDay, AsInt(asInt))

part :: String -> String -> Int
part = genDay part1 part2

type CalibrationData = (Int, [Int])

parser :: String -> [CalibrationData]
parser = map (((,) . asInt . init . head <*> map asInt . tail) . words) . lines

isPossible :: CalibrationData -> ([Int] -> [Int]) -> Bool
isPossible (sol, terms) f = elem sol $ f terms

calculate :: Int -> [Int -> Int -> Int] -> [Int] -> [Int]
calculate prev funcs (x:xs) = concatMap (\f -> calculate (f prev x) funcs xs) funcs
calculate value _ [] = [value]

generalSolution :: ([Int] -> [Int]) -> String -> Int
generalSolution f xs = sum $ map (\x -> if isPossible x f then fst x else 0) $ parser xs

-- triple cuz too lazy to do other tom foolery
-- essentially the `||` operator in the AoC question
(|||) :: Int -> Int -> Int
x ||| y = intCat' (0 :: Int)
  where intCat' n = if (y `div` 10^n) == 0 then y+x*10^n else intCat' (n+1)

part1 :: String -> Int
part1 = generalSolution $ calculate 0 [(+), (*)]

part2 :: String -> Int
part2 = generalSolution $ calculate 0 [(+), (*), (|||)]