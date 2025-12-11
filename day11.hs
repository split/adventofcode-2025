#!/usr/bin/env runghc
module Main where

import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S

type Grid = M.Map String [String]

main = interact (unlines . sequence [part1] . parse)

part1 = ("Part 1: " ++) . show . outs

outs :: Grid -> Int
outs g = go S.empty "you"
  where
    go seen to
      | to == "out" = 1
      | otherwise = sum (go (S.insert to seen) <$> g M.! to)

parse :: String -> Grid
parse = M.mapKeys init . M.fromList . mapMaybe (uncons . words) . lines