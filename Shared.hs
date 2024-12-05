{-# LANGUAGE Safe #-}

module Shared where

import Data.Char (digitToInt)

genDay :: p -> p -> String -> p
genDay part1 part2 x | x == "1" = part1 | x == "2" = part2 | otherwise = error "Only 2 parts exist"

class AsInt a where
  asInt :: a -> Int

instance AsInt String where
  asInt = read

instance AsInt Bool where
  asInt True = 1
  asInt False = 0

instance AsInt Char where
  asInt = digitToInt

takeBack :: Int -> [a] -> [a]
takeBack n str = reverse $ take n $ reverse str

charIsDigit :: Char -> Bool
charIsDigit c = c `elem` "0123456789"

-- https://stackoverflow.com/a/22050875
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x : y : xs) = x <= y && isSorted (y : xs)

-- https://wiki.haskell.org/How_to_work_on_lists#Deleting

remByIdx :: [a] -> Int -> [a]
remByIdx xs n = let (ys, zs) = splitAt n xs in ys ++ tail zs