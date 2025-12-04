#!/usr/bin/env runghc
module Main where

import Data.Set qualified as S

type Grid = S.Set (Int, Int)

main = interact (unlines . sequence [part1, part2] . grid . lines)

part1, part2 :: Grid -> String
part1 = ("Part 1: " ++) . show . S.size . runForklift
part2 = ("Part 2: " ++) . show . countRemoved

countRemoved :: Grid -> Int
countRemoved grid = case runForklift grid of
  removed | null removed -> 0
  removed -> S.size removed + countRemoved (grid S.\\ removed)

runForklift :: Grid -> Grid
runForklift grid = S.filter (\roll -> (< 4) . S.size $ grid `S.intersection` adjacent roll) grid

adjacent p@(x, y) = S.fromList [a | dx <- [-1 .. 1], dy <- [-1 .. 1], let a = (x + dx, y + dy), a /= p]

grid rows = S.fromList [(x, y) | (y, cols) <- zip [0 ..] rows, (x, v) <- zip [0 ..] cols, v == '@']