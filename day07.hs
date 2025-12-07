#!/usr/bin/env runghc
module Main where

import Data.IntMap qualified as M

main = interact (unlines . sequence [part1, part2] . map parse . lines)

part1, part2 :: [M.IntMap Int] -> String
part1 = ("Part 1: " ++) . show . count
part2 = ("Part 2: " ++) . show . sum . M.elems . foldl1 manifold

count [_] = 0
count input@(beams : (xs : rest)) = M.size (beams M.\\ beams') + count (beams' : rest)
  where
    beams' = manifold beams xs

manifold beams xs = M.fromListWith (+) (M.toList (beams M.\\ stopped) ++ split stopped)
  where
    stopped = beams `M.intersection` xs
    split = concatMap (\(k, v) -> [(k - 1, v), (k + 1, v)]) . M.toList

parse xs = M.fromList [(x, 1) | (x, v) <- zip [0 ..] xs, v /= '.']