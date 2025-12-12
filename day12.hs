#!/usr/bin/env runghc
module Main where

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)

main = interact (unlines . sequence [part1] . parse)

part1 = ("Part 1: " ++) . show . count

count (presens, trees) = length (filter fits trees)
  where
    fits (area, qs) = sum (zipWith (*) presens qs) <= area

parse =
  (map present . init &&& map (tree . words) . lines . last)
    . splitOn "\n\n"
  where
    present = length . filter (== '#') . drop 1
    tree (s : qs) = (area (init s), read <$> qs)
    area = product . map read . splitOn "x"