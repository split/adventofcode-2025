#!/usr/bin/env runghc
module Main where

import Data.List (transpose)

main = interact (unlines . sequence [part1] . parse)

part1 = ("Part 1: " ++) . show . sum . map (uncurry foldr1)

parse :: String -> [(Int -> Int -> Int, [Int])]
parse = map (op . reverse) . transpose . map words . lines
  where
    op ("+" : xs) = ((+), map read xs)
    op ("*" : xs) = ((*), map read xs)
    op _ = error "Invalid input"