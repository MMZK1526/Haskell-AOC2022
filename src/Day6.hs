{-# LANGUAGE OverloadedStrings #-}

-- Question source: https://adventofcode.com/2022/day/6

import           Control.Monad.Trans.State
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import           Utilities

data StrCache = StrCache { charMap  :: Map Char Int
                         , curIndex :: Int
                         , count    :: Int }

firstNUnique :: Int -> String -> Int
firstNUnique k str = evalState (worker str) (StrCache M.empty 0 0)
  where
    worker []       = gets curIndex
    worker (x : xs) = do
      cMap  <- gets charMap
      curIx <- gets curIndex
      curC  <- gets count
      if curC == k
        then pure curIx
        else do
          let nextC = case cMap M.!? x of
                Nothing -> curC + 1
                Just ix -> min (curC + 1) (curIx - ix)
          put $ StrCache (M.insert x curIx cMap) (curIx + 1) nextC
          worker xs

day6Part1 :: String -> Int
day6Part1 = firstNUnique 4

day6Part2 :: String -> Int
day6Part2 = firstNUnique 14

main :: IO ()
main = do
  input <- T.unpack <$> readInput "day6"
  print $ day6Part1 input
  print $ day6Part2 input
