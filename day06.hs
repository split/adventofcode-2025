#!/usr/bin/env runghc
module Main where

import Data.Char (isDigit, isSpace)
import Data.List (find, transpose)
import Data.List.Split (splitWhen)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

main = interact (unlines . sequence [part1, part2] . columns . lines)

part1, part2 :: [[String]] -> String
part1 = ("Part 1: " ++) . show . sum . map (calc . transpose)
part2 = ("Part 2: " ++) . show . sum . map calc

calc xs = case find (`elem` "+*") (unlines xs) of
  Just '*' -> product digits
  Just '+' -> sum digits
  where
    digits = mapMaybe (readMaybe . filter isDigit) xs

columns = splitWhen (all isSpace) . transpose