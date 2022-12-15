{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- Question source: https://adventofcode.com/2022/day/14

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Bifunctor
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Tuple
import           Utilities
import qualified Gadgets.Array.Mutable as A
import qualified Gadgets.Array.ST as A

mkRocks :: [Text] -> ([[(Int, Int)]], ((Int, Int), (Int, Int)))
mkRocks strs = (rocks, ( (min (minimum xs - 1) $ 500 - depth, 0)
                       , (max (maximum xs + 1) $ 500 + depth, depth)) )
  where
    worker str = bimap readInt readInt . breakOn "," <$> T.splitOn " -> " str
    rocks      = worker <$> strs
    (xs, ys)   = unzip $ concat rocks
    depth      = maximum ys + 2

mkWorld :: [[(Int, Int)]] -> ((Int, Int), (Int, Int)) -> Array (Int, Int) Bool
mkWorld rockSchema bds = runST $ do
  waterfallST <- A.newArray bds False
  forM_ rockSchema $ \rock -> forM_ (zip rock (tail rock)) $ \seg ->
    forM_ (range seg ++ range (swap seg)) $ \pos -> waterfallST A.=: pos $ True
  A.freeze waterfallST

simulateOne :: STArray s (Int, Int) Bool -> ST s Bool
simulateOne waterfallST = worker (500, 0)
  where
    fall pos@(x, y) = do
      result <- listToMaybe <$> filterM (fmap not . (waterfallST A.!))
                                [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
      when (isNothing result) (waterfallST A.=: pos $ True)
      return result
    worker (x, y)   = do
      yMax <- snd . snd <$> getBounds waterfallST
      if y == yMax
        then pure True
        else fall (x, y) >>= \case
          Nothing  -> pure False
          Just pos -> worker pos

day14Part1 :: Array (Int, Int) Bool -> Int
day14Part1 waterfall = runST $ do
  waterfallST <- A.thaw waterfall
  let worker i = do
        isFinished <- simulateOne waterfallST
        if isFinished then pure i else worker (i + 1)
  worker 0

day14Part2 :: Array (Int, Int) Bool -> Int
day14Part2 waterfall = runST $ do
  waterfallST               <- A.thaw waterfall
  ((xMin, _), (xMax, yMax)) <- getBounds waterfallST
  forM_ [xMin..xMax] $ \x -> waterfallST A.=: (x, yMax) $ True
  let worker i = do
        simulateOne waterfallST
        isOriginFilled <- waterfallST A.! (500, 0)
        if isOriginFilled then pure i else worker (i + 1)
  worker 1

main :: IO ()
main = do
  waterfall <- uncurry mkWorld . mkRocks . T.lines <$> readInput "day14"
  print $ day14Part1 waterfall
  print $ day14Part2 waterfall
