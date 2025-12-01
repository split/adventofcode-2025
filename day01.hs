#!/usr/bin/env runghc
module Main where

main = interact (unlines . sequence [part1] . map ro . lines)

part1 = ("Part 1: " ++) . show . length . filter (== 0) . scanl step 50

step acc x = rem (100 + acc + x) 100

ro :: String -> Int
ro ('L' : xs) = -read xs
ro ('R' : xs) = read xs
ro _ = error "Invalid rotation"
