#!/usr/bin/env runghc
module Main where

import Control.Monad (msum)
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

type Point = (Int, Int, Int)

main = interact (unlines . sequence [part1, part2] . parse)

part1 :: [Point] -> String
part1 = ("Part 1: " ++) . show . score . go
  where
    go xs = foldl (flip joinCircuits) (circuits xs) (takeN (closest xs))
    takeN xs = take (if length xs > 1000 then 1000 else 10) xs
    score = product . take 3 . sortOn negate . map S.size

part2 :: [Point] -> String
part2 = ("Part 2: " ++) . show . fromMaybe 0 . msum . map match . go
  where
    go xs = (zip <*> drop 1 . scanl (flip joinCircuits) (circuits xs)) (closest xs)
    match (p@((x, _, _), (x', _, _)), circuits) = case circuits of
      [_] -> Just (x * x')
      _ -> Nothing

joinCircuits :: (Point, Point) -> [S.Set Point] -> [S.Set Point]
joinCircuits (q, p) = join S.empty
  where
    conn = S.fromList [p, q]
    join acc [] = [acc]
    join acc (cir : xs)
      | null (conn `S.intersection` cir) = cir : join acc xs
      | otherwise = join (cir <> acc) xs

circuits = map S.singleton

closest = sortOn distSq . pairs
  where
    distSq ((x, y, z), (x', y', z')) = (x' - x) ^ 2 + (y' - y) ^ 2 + (z' - z) ^ 2

pairs [] = []
pairs (x : xs) = [(x, y) | y <- xs] ++ pairs xs

parse :: String -> [Point]
parse = map (read . (\x -> "(" ++ x ++ ")")) . lines