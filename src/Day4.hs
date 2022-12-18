{-# LANGUAGE OverloadedStrings #-}

-- Question source: https://adventofcode.com/2022/day/4

import           Data.Bifunctor
import           Data.Text (Text)
import qualified Data.Text as T
import           Utilities

parseSegment :: Text -> ((Int, Int), (Int, Int))
parseSegment = let worker = bimap readInt readInt . breakOn "-"
               in  bimap worker worker . breakOn ","

day4Part1 :: [((Int, Int), (Int, Int))] -> Int
day4Part1
  = sumWorker
  $ \((a, b), (c, d)) -> fromEnum ((a <= c && b >= d) || (a >= c && b <= d))

day4Part2 :: [((Int, Int), (Int, Int))] -> Int
day4Part2 = sumWorker $ \((a, b), (c, d)) -> fromEnum (not $ b < c || d < a)

main :: IO ()
main = do
  input <- map parseSegment . T.lines <$> readInput "day4"
  print $ day4Part1 input
  print $ day4Part2 input
