module Day3 (part) where

import Data.Char (isDigit)
import Shared (genDay, takeBack)

part :: String -> String -> Int
part = genDay part1 part2

parser :: [Char] -> Bool -> String -> Int
parser _ True xs =
  let (digits, after) = span isDigit xs
      num1 = if not (null digits) then read digits :: Int else 0
   in if not (null after) && head after /= ','
        then parser [] False (tail after)
        else
          let (digits2, after2) = span isDigit $ tail after
              num2 = if not (null digits2) then read digits2 :: Int else 0
           in if length after2 > 1 && head after2 /= ')'
                then parser [] False (tail after2)
                else num1 * num2 + if not (null after2) then parser [] False (tail after2) else 0
parser pre False ('(' : xs) = parser [] (take 3 (reverse pre) == "lum") xs
parser pre False (x : xs) = parser (pre ++ [x]) False xs
parser _ _ [] = 0

parse2Pre :: [Char] -> Bool -> Bool
parse2Pre pre prevShould
  | takeBack 5 pre == "don't" = False
  | takeBack 2 pre == "do" = True
  | otherwise = prevShould

parser2 :: [Char] -> Bool -> Bool -> String -> Int
parser2 _ True True xs =
  let (digits, after) = span isDigit xs
      num1 = if not (null digits) then read digits :: Int else 0
   in if not (null after) && head after /= ','
        then parser2 [] False True (tail after)
        else
          let (digits2, after2) = span isDigit $ tail after
              num2 = if not (null digits2) then read digits2 :: Int else 0
           in if length after2 > 1 && head after2 /= ')'
                then parser2 [] False True (tail after2)
                else num1 * num2 + if not (null after2) then parser2 [] False True (tail after2) else 0
parser2 pre True False (x : xs) = parser2 (pre ++ [x]) False False xs
parser2 _ False should (')' : xs) = parser2 [] False should xs
parser2 pre False should ('(' : xs) = parser2 [] (take 3 (reverse pre) == "lum") (parse2Pre pre should) xs
parser2 pre False should (x : xs) = parser2 (pre ++ [x]) False should xs
parser2 _ _ _ [] = 0

part1 :: String -> Int
part1 = parser [] False

part2 :: String -> Int
part2 = parser2 [] False True
