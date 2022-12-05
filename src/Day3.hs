{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Question source: https://adventofcode.com/2022/day/3

import           Data.Bifunctor
import           Data.Char
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Utilities

toSet :: Text -> Set Char
toSet = S.fromList . T.unpack

parseRucksack :: Text -> (Set Char, Set Char)
parseRucksack str = bimap toSet toSet $ T.splitAt (T.length str `div` 2) str

parseRucksack2 :: [Text] -> [(Set Char, Set Char, Set Char)]
parseRucksack2 (t1 : t2 : t3 : ts) = (toSet t1, toSet t2, toSet t3)
                                   : parseRucksack2 ts
parseRucksack2 []                  = []

getPriority :: Char -> Int
getPriority ch
  | 'a' <= ch && ch <= 'z' = ord ch - ord 'a' + 1
  | 'A' <= ch && ch <= 'Z' = ord ch - ord 'A' + 27

day3Part1 :: [(Set Char, Set Char)] -> Int
day3Part1 = sumWorker $ getPriority . S.elemAt 0 . uncurry S.intersection

day3Part2 :: [(Set Char, Set Char, Set Char)] -> Int
day3Part2 = sumWorker $ \(p1, p2, p3) ->
  getPriority . S.elemAt 0 $ foldr1 S.intersection [p1, p2, p3]

main :: IO ()
main = do
  raw <- T.lines <$> readInput "day3"
  let input1 = parseRucksack <$> raw
  let input2 = parseRucksack2 raw
  print $ day3Part1 input1
  print $ day3Part2 input2
