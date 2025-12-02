#!/usr/bin/env runghc

module Main where

import Data.List.Split (splitOn)

main = interact (unlines . sequence [part1, part2] . concat . concatMap parse . lines)

part1 = ("Part 1: " ++) . show . sum . filter (elem 2 . repeats . show)
part2 = ("Part 2: " ++) . show . sum . filter (not . null . repeats . show)

repeats :: (Eq a) => [a] -> [Int]
repeats xs = go 1
 where
  n = length xs
  go ptr
    | ptr == n = []
    | n `mod` ptr /= 0 = go (ptr + 1)
    | concat (replicate (n `div` ptr) (take ptr xs)) == xs = n `div` ptr : go (ptr + 1)
    | otherwise = go (ptr + 1)

parse = map (range . splitOn "-") . splitOn ","

range :: [String] -> [Int]
range (s : e : _) = [read s .. read e]
