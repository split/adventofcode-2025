#!/usr/bin/env runghc
module Main where

import Data.List (sortOn)
import Data.List.Split (splitOn)

type Range = (Int, Int)

main = interact (unlines . sequence [part1, part2] . parse)

part1 = ("Part 1: " ++) . show . length . filter id . uncurry (map . inRanges)

part2 = ("Part 2: " ++) . show . sum . map count . mergeAll . fst

count (l, r) = r - l + 1

mergeAll = foldr step [] . sortOn fst
  where
    merge (l1, u1) (l2, u2) = (l1 `min` l2, u1 `max` u2)
    overlaps (l1, u1) (l2, u2) = not (u1 < l2 || u2 < l1)
    step r [] = [r]
    step r acc@(x : xs)
      | r `overlaps` x = step (merge r x) xs
      | otherwise = r : acc

inRanges rr v = any (v `inRange`) rr

inRange v (l, u) = l <= v && v <= u

parse :: String -> ([(Int, Int)], [Int])
parse = parts . map lines . splitOn "\n\n"
  where
    parts [r, a] = (map range r, map read a)
    range = (\[l, u] -> (read l, read u)) . splitOn "-"