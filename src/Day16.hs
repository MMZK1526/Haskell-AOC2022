{-# LANGUAGE OverloadedStrings #-}

-- Question source: https://adventofcode.com/2022/day/16

import           Control.Monad
import           Control.Monad.Trans.State
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

mkCache :: Int -> Map Int (Int, [(Int, Int)]) -> Map Int Int
mkCache time caves = execState (buildCache time 0 0 0) M.empty
  where
    buildCache rem pos c acc = when (rem > 1) $ do
      let acc' = acc + fst (caves M.! pos) * rem
      when (pos > 0) $ modify' (M.insertWith max c acc')
      forM_ (snd (caves M.! pos)) $ \(pos', cost) -> unless (testBit c pos') $
        buildCache (rem - 1 - cost) pos' (setBit c pos) acc'

day16Part1 :: Map Int (Int, [(Int, Int)]) -> Int
day16Part1 caves = maximum (mkCache 30 caves)

day16Part2 :: Map Int (Int, [(Int, Int)]) -> Int
day16Part2 caves
  = maximum [cache M.! i + cache M.! j | i <- keys, j <- keys, i .&. j == 1]
  where
    cache = mkCache 26 caves
    keys  = M.keys cache

main :: IO ()
main = do
  caves <- mkCaves . T.lines <$> readInput "day16"
  print $ day16Part1 caves
  print $ day16Part2 caves
