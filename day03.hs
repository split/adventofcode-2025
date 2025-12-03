#!/usr/bin/env runghc
module Main where

import Data.Char (digitToInt)

main = interact (unlines . sequence [part1] . banks)

part1 = ("Part 1: " ++) . show . sum . map (maximum . joltages)

joltages [] = []
joltages (x : xs) = ((10 * x +) <$> xs) ++ joltages xs

banks = map (map digitToInt) . lines