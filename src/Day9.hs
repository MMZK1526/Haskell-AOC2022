{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Question source: https://adventofcode.com/2022/day/9

import           Control.Monad.Trans.State
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import           Utilities

data RopeMap = RopeMap { poses :: [(Int, Int)], touched :: Set (Int, Int) }

newRope :: Int -> RopeMap
newRope len = RopeMap (replicate len (0, 0)) $ S.singleton (0, 0)

moveRope :: RopeMap -> (Int, Int) -> RopeMap
moveRope r@RopeMap { poses = ((hx, hy) : poses') } (dx, dy) 
  = r { poses = (hx + dx, hy + dy) : poses''
      , touched = S.insert lst (touched r) }
  where
    hd                 = (hx + dx, hy + dy)
    (poses'', lst)     = runState (mapM adjustOne poses') hd
    adjustOne (tx, ty) = do
      (xDiff, yDiff) <- fmap (\(hx, hy) -> (hx - tx, hy - ty)) get
      put $ if | abs xDiff >= 2 -> (tx + signum xDiff, ty + signum yDiff)
               | abs yDiff >= 2 -> (tx + signum xDiff, ty + signum yDiff)
               | otherwise      -> (tx, ty)
      get

toInstrs :: [String] -> [(Int, Int)]
toInstrs = concatMap $ \case
  'D' : _ : n -> replicate (read n) (0, -1)
  'U' : _ : n -> replicate (read n) (0, 1)
  'L' : _ : n -> replicate (read n) (-1, 0)
  'R' : _ : n -> replicate (read n) (1, 0)

day9Part1 :: [(Int, Int)] -> Int
day9Part1 = length . touched . foldl moveRope (newRope 2)

day9Part2 :: [(Int, Int)] -> Int
day9Part2 = length . touched . foldl moveRope (newRope 10)

main :: IO ()
main = do
  instrs <- toInstrs . fmap T.unpack . T.lines <$> readInput "day9"
  print $ day9Part1 instrs
  print $ day9Part2 instrs
