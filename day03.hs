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

joltage = joltage' 0

joltage' :: Int -> Int -> [Int] -> Int
joltage' acc n bank
  | n <= 0 = acc
  | otherwise = joltage' (acc * 10 + j) (n - 1) bank'
  where
    i = max 1 (length bank - n + 1)
    (j : bank') = maximumBy (comparing (take 1 &&& length)) (take i (tails bank))

banks = map (map digitToInt) . lines