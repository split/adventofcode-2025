#!/usr/bin/env runghc
module Main where

main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: [(Int, Int)] -> String
part1 = ("Part 1: " ++) . show . maximum . map area . pairs
part2 = ("Part 2: " ++) . show . maximum . map area . (filter . flip inside <*> pairs)

inside ((x, y), (x', y')) = all is . edges
  where
    is ((lx, ly), (rx, ry)) =
      max lx rx <= min x x' || min lx rx > max x x' || max ly ry <= min y y' || min ly ry >= max y y'

area ((x, y), (x', y')) = (1 + abs (x - x')) * (1 + abs (y - y'))

pairs [] = []
pairs (x : xs) = [(x, y) | y <- xs] ++ pairs xs

edges = zip . cycle <*> drop 1

parse = map (read . (\x -> "(" ++ x ++ ")")) . lines