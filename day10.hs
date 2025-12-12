#!/usr/bin/env runghc
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (bimap)
import Data.Bits (Bits (bit, shiftL, shiftR, xor, (.&.), (.|.)))
import Data.List (foldl', uncons, zipWith)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set qualified as S

data Machine = Machine
  {presses :: Int, lights :: [Int], buttons :: Seq [Int], jotages :: [Int]}
  deriving (Show, Eq, Ord)

main = interact (unlines . sequence [part1, part2] . parse)

part1 :: [Machine] -> String
part1 = ("Part 1: " ++) . show . sum . map (presses . runMachine)

part2 = ("Part 2: " ++) . show

runMachine :: Machine -> Machine
runMachine im@Machine {buttons} = press ((im,) <$> buttons) S.empty
  where
    press ((m, b) :<| next) seen
      | (lights m, b) `S.member` seen = press next seen
      | allON (lights m) = m
      | otherwise =
          press
            (next >< ((pressButton m b,) <$> buttons))
            ((lights m, b) `S.insert` seen)

pressButton m b = m {presses = presses m + 1, lights = toggle b (lights m)}

toggle b = zipWith (\i l -> if i `elem` b then (l + 2) `xor` 1 else l) [0 ..]

allON = all (\v -> (v .&. 1) == 1)

parse = map (\(l, v) -> Machine 0 l (Seq.fromList $ init v) (last v)) . mapMaybe go . lines . concatMap fmt
  where
    go = (lights `bimap` map read <$>) . uncons . words
    lights = map (\case '#' -> 0; '.' -> 1)
    fmt = \case
      c | c `elem` "[]" -> ""
      c | c `elem` "{(" -> "["
      c | c `elem` "})" -> "]"
      c -> [c]
