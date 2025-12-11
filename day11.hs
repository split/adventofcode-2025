#!/usr/bin/env runghc
module Main where

import Control.Monad.State (evalState, gets, modify)
import Data.List (uncons)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)

type Grid = M.Map String [String]

main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: Grid -> String
part1 = ("Part 1: " ++) . show . ("you" `outs` "out")
part2 = ("Part 2: " ++) . show . product . applyAll ["svr" `outs` "fft", "fft" `outs` "dac", "dac" `outs` "out"]

applyAll fs g = map ($ g) fs

outs :: String -> String -> Grid -> Int
outs from to g = evalState (go from) M.empty
  where
    go = memoState go'
    go' cur
      | cur == to = pure 1
      | cur `M.member` g = sum <$> mapM go (g M.! cur)
      | otherwise = pure 0

parse :: String -> Grid
parse = M.mapKeys init . M.fromList . mapMaybe (uncons . words) . lines

memoState f k = maybe compute pure =<< gets (M.lookup k)
  where
    compute = f k >>= \v -> modify (M.insert k v) >> pure v