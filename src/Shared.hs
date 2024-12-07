{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Shared (genDay, AsInt(asInt), takeBack, isSorted, remByIdx, BTree(Node, Leaf), leaves) where

import Data.Char (digitToInt)

genDay :: p -> p -> String -> p
genDay part1 part2 x | x == "1" = part1 | x == "2" = part2 | otherwise = error "Only 2 parts exist"

class AsInt a where
  asInt :: a -> Int

instance (a ~ Char) => AsInt [a] where
  asInt = read

instance AsInt Bool where
  asInt True = 1
  asInt False = 0

instance AsInt Char where
  asInt = digitToInt

takeBack :: Int -> [a] -> [a]
takeBack n str = reverse $ take n $ reverse str

-- https://stackoverflow.com/a/22050875
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : xs) = x <= y && isSorted (y : xs)

-- https://wiki.haskell.org/How_to_work_on_lists#Deleting

remByIdx :: [a] -> Int -> [a]
remByIdx xs n = let (ys, zs) = splitAt n xs in ys ++ tail zs

-- https://stackoverflow.com/questions/10592920/haskell-flatten-binary-tree
data BTree a = Node (BTree a) a (BTree a) | Leaf a

leaves :: BTree a -> [a]
leaves (Node left mid right) = leaves left ++ leaves right
leaves (Leaf val) = [val]