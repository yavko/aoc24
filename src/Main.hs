{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Day1 qualified as D1
import Day2 qualified as D2
import Day3 qualified as D3
import Day7 qualified as D7
import System.Environment ( getArgs )
import System.Exit (exitSuccess, exitFailure)

main :: IO b
main = getArgs >>= parse

readInput :: [Char] -> IO String
readInput day = readFile ("./inputs/" ++ day ++ ".txt")

chooseDay :: String -> (String -> String -> Int)
chooseDay day
  | day == "1" = D1.part
  | day == "2" = D2.part
  | day == "3" = D3.part
  | day == "7" = D7.part 
  | otherwise = error ("Day " ++ show day ++ " not implemented")

parse :: [String] -> IO b
parse ["-d", day, "-p", part] = readInput day >>= \input -> print (chooseDay day part input) >> exitSuccess
parse [] = print "empty input" >> exitFailure
parse x = error ("Invalid Input: " ++ show x) >> exitFailure
