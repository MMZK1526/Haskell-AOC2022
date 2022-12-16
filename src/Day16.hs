{-# LANGUAGE OverloadedStrings #-}

-- Question source: https://adventofcode.com/2022/day/16

import           Data.Array
import           Data.Bifunctor
import           Data.Bits
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Gadgets.Array
import           Utilities

mkCaves :: [Text] -> Map Int (Int, [(Int, Int)])
mkCaves strs = M.fromList $ toIndex <$> M.keys compactMap
  where
    toIndex k  = (indices M.! k, second (map (first (indices M.!))) $ compactMap M.! k)
    indices    = M.fromList $ zip (fst <$> sortOn (fst . snd) (M.toList compactMap)) [0..]
    compactMap = M.fromList $ concatMap compress (M.keys tempMap)
    compress k
      | fst (tempMap M.! k) > 0 || k == "AA" = [(k, (fst (tempMap M.! k), go (S.singleton k) . zip (snd (tempMap M.! k)) $ repeat 1))]
      | otherwise                            = []
    go _ []    = []
    go vis ((fr, dist) : frs)
      | fr `elem` vis             = go vis frs
      | fst (tempMap M.! fr) /= 0 = (fr, dist) : go (S.insert fr vis) (frs ++ [(fr', dist + 1) | fr' <- snd (tempMap M.! fr)])
      | otherwise                 = go (S.insert fr vis) (frs ++ [(fr', dist + 1) | fr' <- snd (tempMap M.! fr)])
    tempMap    = M.fromList $ worker <$> strs
    worker str = (name, (value, caves))
      where
        (this, others) = bimap T.words (breakOn " ") $ breakOn "; " str
        (name, value)  = (this !! 1, readInt . snd . breakOn "=" $ last this)
        caves          = T.splitOn ", " . snd $ case fst others of
          "tunnel" -> breakOn "valve " $ snd others
          _        -> breakOn "valves " $ snd others

mkCache :: Map Int (Int, [(Int, Int)]) -> (Array (Int, Int, Int) Int, Int)
mkCache caves = (memoise, maxConfig)
  where
    memoise   = tabulate ((1, 0, 0), (30, length caves - 1, maxConfig)) worker
    maxConfig = shiftL (1 :: Int) (length caves - 1) - 1
    get entry@(rem, ix, c)
      | rem <= 1  = 0
      | otherwise = memoise ! entry
    worker (rem, ix, c)
      | rem <= 1 || c == maxConfig = 0
      | ix >= 1 = m2 + (rem - 1) * fst (caves M.! ix)
      | otherwise                                   = m1
      where
        m1 = maximum $ 0 : [get (rem - t, x, c) | (x, t) <- snd (caves M.! ix), not (testBit c (x - 1))]
        m2 = maximum $ 0 : [get (rem - 1 - t, x, c') | (x, t) <- snd (caves M.! ix), not (testBit c' (x - 1))]
        c' = setBit c (ix - 1)

day16Part1 :: Array (Int, Int, Int) Int -> Int
day16Part1 = (! (30, 0, 0))

day16Part2 :: (Array (Int, Int, Int) Int, Int) -> Int
day16Part2 (cache, maxConfig) = maximum [cache ! (26, 0, i) + cache ! (26, 0, maxConfig - i) | i <- [0..(maxConfig `div` 2)]]

main :: IO ()
main = do
  caves <- mkCaves . T.lines <$> readInput "day16"
  let cache = mkCache caves
  print $ day16Part1 (fst cache)
  print $ day16Part2 cache
