#!/usr/bin/env runghc
module Main where

main = interact (unlines . sequence [part1] . parse)

part1 = ("Part 1: " ++) . show . maximum . map area . pairs

area ((x, y), (x', y')) = abs (x - x' + 1) * abs (y - y' + 1)

pairs [] = []
pairs (x : xs) = [(x, y) | y <- xs] ++ pairs xs

parse :: String -> [(Int, Int)]
parse = map (read . (\x -> "(" ++ x ++ ")")) . lines