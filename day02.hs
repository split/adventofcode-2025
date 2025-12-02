#!/usr/bin/env runghc

module Main where

import Data.List.Split (splitOn)

main = interact (unlines . sequence [part1] . concat . concatMap parse . lines)

part1 = ("Part 1: " ++) . show . sum . filter (twice . show)

twice :: (Eq a) => [a] -> Bool
twice xs = case length xs of
  n | even n -> and (zipWith (==) xs (drop (length xs `div` 2) xs))
  _ -> False

parse = map (range . splitOn "-") . splitOn ","

range :: [String] -> [Int]
range (s : e : _) = [read s .. read e]
