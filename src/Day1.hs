{-# LANGUAGE OverloadedStrings  #-}

-- Question source: https://adventofcode.com/2022/day/1

import qualified Numeric as N
import           Utilities
import           Data.List

day1Part1 :: [[Integer]] -> Integer
day1Part1 = maximum . map sum

day1Part2 :: [[Integer]] -> Integer
day1Part2 = sum . take 3 . sortOn negate . map sum

main :: IO ()
main = do
  input <- fmap readInts . readGroups <$> readInput "day1"
  print $ day1Part1 input
  print $ day1Part2 input
