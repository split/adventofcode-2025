#!/usr/bin/env runghc
module Main where

main = interact (unlines . sequence [part1, part2] . map ro . lines)

part1 = ("Part 1: " ++) . show . clicks
part2 = ("Part 2: " ++) . show . clicks . method_0x434C49434B

method_0x434C49434B xs = [signum r | r <- xs, x <- [1 .. abs r]]

clicks = length . filter (== 0) . scanl step 50
 where
  step acc x = rem (100 + acc + x) 100

ro :: String -> Int
ro ('L' : xs) = -read xs
ro ('R' : xs) = read xs
ro _ = error "Invalid rotation"
