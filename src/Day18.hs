{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- Question source: https://adventofcode.com/2022/day/18

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Gadgets.Array.Mutable as A
import qualified Gadgets.Array.ST as A
import           Utilities

data Shape = Water | Lava | Air
  deriving Eq

mkBoxes :: [Text] -> ([(Int, Int, Int)], Array (Int, Int, Int) Shape)
mkBoxes strs = (points ,) $ runST $ do
  boxesST <- A.newArray ((-1, -1, -1), (22, 22, 22)) Air
  forM_ points $ flip (boxesST A.=:) Lava
  A.freeze boxesST
  where
    points     = worker <$> strs
    worker str = let [x, y, z] = T.splitOn "," str
                 in  (readInt x, readInt y, readInt z)

expand :: (Int, Int, Int) -> [(Int, Int, Int)]
expand (x, y, z) = filter (inRange ((-1, -1, -1), (22, 22, 22)))
                          [ (x - 1, y, z), (x + 1, y, z), (x, y - 1, z)
                          , (x, y + 1, z), (x, y, z - 1), (x, y, z + 1) ]

day18Part1 :: ([(Int, Int, Int)], Array (Int, Int, Int) Shape) -> Int
day18Part1 (points, boxes)
  = sum $ map (length . filter ((== Air) . (boxes !)) . expand) points

day18Part2 :: Array (Int, Int, Int) Shape -> Int
day18Part2 boxes = runST $ do
  boxesST <- A.thaw boxes
  let worker acc []       = pure acc
      worker acc (p : ps) = do
        nbvs <- let nbs = expand p in zip nbs <$> forM nbs (boxesST A.!)
        let ps' = fst <$> filter ((== Air) . snd) nbvs
        forM_ ps' $ flip (boxesST A.=:) Water
        worker (acc + length (filter ((== Lava) . snd) nbvs)) (ps' ++ ps)
  boxesST A.=: (-1, -1, -1) $ Water
  worker 0 [(-1, -1, -1)]

main :: IO ()
main = do
  input <- mkBoxes . T.lines <$> readInput "day18"
  print $ day18Part1 input
  print . day18Part2 $ snd input
