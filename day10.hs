#!/usr/bin/env runghc
module Main where

import Data.Bifunctor (bimap)
import Data.Bits (Bits (bit, shiftL, shiftR, xor, (.&.), (.|.)))
import Data.List (foldl', uncons, zipWith)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set qualified as S

data Machine = Machine
  {presses :: Int, lights :: Int, buttons :: Seq [Int]}
  deriving (Show, Eq, Ord)

main = interact (unlines . sequence [part1] . parse)

part1 :: [Machine] -> String
part1 = ("Part 1: " ++) . show . sum . map (presses . runMachine)

runMachine :: Machine -> Machine
runMachine im@Machine {buttons} = press ((im,) <$> buttons) S.empty
  where
    press ((m, b) :<| next) seen
      | (lights m, b) `S.member` seen = press next seen
      | matches (lights m) = m
      | otherwise =
          press
            (next >< ((pressButton m b,) <$> buttons))
            ((lights m, b) `S.insert` seen)

pressButton m b = m {presses = presses m + 1, lights = toggle (lights m) b}

toggle lights button = lights `xor` foldl' (.|.) 0 (bit . (* 2) <$> button)

matches v = v `shiftR` 1 == 0 || ((v .&. 1) == 1 && matches (v `shiftR` 2))

parse = map (uncurry (Machine 0)) . mapMaybe go . lines . concatMap fmt
  where
    go = (lights `bimap` (Seq.fromList . init . map read) <$>) . uncons . words
    lights = foldr (\v acc -> acc `shiftL` 2 .|. case v of '#' -> 2; '.' -> 3) 0
    fmt c = case c of
      c | c `elem` "[]" -> ""
      c | c `elem` "{(" -> "["
      c | c `elem` "})" -> "]"
      c -> [c]
