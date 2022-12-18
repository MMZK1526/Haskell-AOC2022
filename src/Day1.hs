{-# LANGUAGE OverloadedStrings #-}

-- Question source: https://adventofcode.com/2022/day/1

import           Data.List
import           Utilities

day1Part1 :: [[Int]] -> Int
day1Part1 = maximum . map sum

day1Part2 :: [[Int]] -> Int
day1Part2 = sum . take 3 . sortOn negate . map sum

main :: IO ()
main = do
  input <- map readInts . readGroups <$> readInput "day1"
  print $ day1Part1 input
  print $ day1Part2 input
