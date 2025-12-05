#!/usr/bin/env runghc
module Main where

import Data.List (sortOn)
import Data.List.Split (splitOn)

type Range = (Int, Int)

main = interact (unlines . sequence [part1, part2] . parse)

part1 = ("Part 1: " ++) . show . length . filter id . uncurry (map . inRanges)

part2 = ("Part 2: " ++) . show . count . fst

count = snd . foldl go (0, 0) . sortOn fst
  where
    go (ptr, acc) (l, u)
      | u > ptr = (u, acc + u - (ptr + 1) `max` l + 1)
      | otherwise = (ptr, acc)

inRanges rr v = any (v `inRange`) rr

inRange v (l, u) = l <= v && v <= u

parse :: String -> ([(Int, Int)], [Int])
parse = parts . map lines . splitOn "\n\n"
  where
    parts [r, a] = (map range r, map read a)
    range = (\[l, u] -> (read l, read u)) . splitOn "-"