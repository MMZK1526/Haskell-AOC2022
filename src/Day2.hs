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
parseStrat str = ( toEnum $ ord (str `T.index` 0) - ord 'A'
                 , toEnum $ ord (str `T.index` 2) - ord 'X' )

day2Part1 :: [(ColStrategy, RowStrategy)] -> Int
day2Part1 = sumWorker work
  where
    work (colS, rowS) = choice + result
      where
        choice = fromEnum rowS + 1
        result = case (fromEnum colS - fromEnum rowS) `mod` 3 of
          2  -> 6
          1  -> 0
          0  -> 3

day2Part2 :: [(ColStrategy, RowStrategy)] -> Int
day2Part2 = sumWorker work
  where
    work (colS, target) = choice + result
      where
        choice = 1 + (fromEnum colS + fromEnum target - 1) `mod` 3
        result = 3 * fromEnum target

main :: IO ()
main = do
  input <- fmap parseStrat . T.lines <$> readInput "day2"
  print $ day2Part1 input
  print $ day2Part2 input
