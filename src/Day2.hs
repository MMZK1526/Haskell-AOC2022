{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Question source: https://adventofcode.com/2022/day/2

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Utilities

data ColStrategy = A | B | C
  deriving Enum

data RowStrategy = X | Y | Z
  deriving Enum

parseStrat :: Text -> (ColStrategy, RowStrategy)
parseStrat str = (colS, rowS)
  where
    colS = case str `T.index` 0 of
      'A' -> A
      'B' -> B
      'C' -> C
    rowS = case str `T.index` 2 of
      'X' -> X
      'Y' -> Y
      'Z' -> Z

day2Part1 :: [(ColStrategy, RowStrategy)] -> Int
day2Part1 = sum . map work
  where
    work (colS, rowS) = choice + result
      where
        choice = fromEnum rowS + 1
        result = case (fromEnum colS - fromEnum rowS) `mod` 3 of
          2  -> 6
          1  -> 0
          0  -> 3

day2Part2 :: [(ColStrategy, RowStrategy)] -> Int
day2Part2 = sum . map work
  where
    work (colS, target) = choice + result
      where
        choice = 1 + (fromEnum colS + fromEnum target - 1) `mod` 3
        result = case target of
          X -> 0
          Y -> 3
          Z -> 6

main :: IO ()
main = do
  input <- fmap parseStrat . T.lines <$> readInput "day2"
  print $ day2Part1 input
  print $ day2Part2 input
