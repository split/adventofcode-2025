#!/usr/bin/env runghc
module Main where

import Control.Arrow ((&&&))
import Data.Char (digitToInt)
import Data.List (maximumBy, tails)
import Data.Ord (comparing)

main = interact (unlines . sequence [part1, part2] . banks)

part1, part2 :: [[Int]] -> String
part1 = ("Part 1: " ++) . show . sum . map (joltage 2)
part2 = ("Part 2: " ++) . show . sum . map (joltage 12)

joltage :: Int -> [Int] -> Int
joltage n = foldl (\x y -> x * 10 + y) 0 . joltage' n

joltage' :: Int -> [Int] -> [Int]
joltage' n banks
  | n <= 0 = []
  | otherwise = j : joltage' (n - 1) banks'
  where
    i = max 1 (length banks - n + 1)
    (j : banks') = maximumBy (comparing (take 1 &&& length)) (take i (tails banks))

banks = map (map digitToInt) . lines