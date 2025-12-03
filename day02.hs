#!/usr/bin/env runghc
module Main where

import Data.List (inits)
import Data.List.Split (splitOn)

main = interact (unlines . sequence [part1, part2] . concat . concatMap parse . lines)

part1, part2 :: [Int] -> String
part1 = ("Part 1: " ++) . show . sum . filter (elem 2 . repeats . show)
part2 = ("Part 2: " ++) . show . sum . filter (not . null . repeats . show)

repeats :: (Eq a) => [a] -> [Int]
repeats xs = foldr go [] (inits xs)
  where
    go part acc
      | ptr < 1 || ptr == len || len `mod` ptr /= 0 = acc
      | and (zipWith (==) xs (cycle part)) = len `div` ptr : acc
      | otherwise = acc
      where
        len = length xs
        ptr = length part

parse = map (range . splitOn "-") . splitOn ","

range :: [String] -> [Int]
range (s : e : _) = [read s .. read e]
