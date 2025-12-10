#!/usr/bin/env runghc
module Main where

import Data.Bifunctor (bimap)
import Data.Bits (Bits (shiftR, xor, (.&.)))
import Data.List (uncons, zipWith)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set qualified as S

data Machine = Machine
  {presses :: Int, lights :: [Int], buttons :: [[Int]]}
  deriving (Show, Eq, Ord)

main = interact (unlines . sequence [part1] . parse)

part1 :: [Machine] -> String
part1 = ("Part 1: " ++) . show . sum . map (presses . runMachine)

runMachine :: Machine -> Machine
runMachine im@Machine {buttons} = press ((im,) <$> Seq.fromList buttons) S.empty
  where
    press ((m, b) :<| next) seen
      | all matches (lights m) = m
      | (b, lights m) `S.member` seen = press next seen
      | otherwise =
          press
            (next >< Seq.fromList ((pressButton m b,) <$> buttons))
            ((b, lights m) `S.insert` seen)

pressButton m button = m {presses = presses m + 1, lights = zipWith (toggle button) [0 ..] (lights m)}

toggle :: [Int] -> Int -> Int -> Int
toggle button i state
  | i `elem` button = state `xor` 1
  | otherwise = state

matches v = ((v `xor` (v `shiftR` 1)) .&. 1) == 0

parse = map (uncurry (Machine 0)) . mapMaybe go . lines . concatMap fmt
  where
    go = (map light `bimap` (init . map read) <$>) . uncons . words
    light v = case v of
      '#' -> 2
      '.' -> 0
    fmt c = case c of
      c | c `elem` "[]" -> ""
      c | c `elem` "{(" -> "["
      c | c `elem` "})" -> "]"
      c -> [c]
