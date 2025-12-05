#!/usr/bin/env runghc
module Main where

import Data.List.Split (splitOn)

main = interact (unlines . sequence [part1] . parse)

part1 = ("Part 1: " ++) . show . length . filter id . uncurry (map . inRanges)

inRanges rr v = any (`inRange` v) rr

inRange (l, u) v = l <= v && v <= u

parse :: String -> ([(Int, Int)], [Int])
parse = parts . map lines . splitOn "\n\n"
  where
    parts [r, a] = (map range r, map read a)
    range = (\[l, u] -> (read l, read u)) . splitOn "-"