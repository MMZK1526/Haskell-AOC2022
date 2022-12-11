{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Question source: https://adventofcode.com/2022/day/11

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Bifunctor
import           Data.List
import           Data.Sequence (Seq)
import qualified Data.Sequence as L
import           Data.Text (Text)
import qualified Data.Text as T
import           Gadgets.Array
import qualified Gadgets.Array.Mutable as A
import qualified Gadgets.Array.ST as A
import           Utilities

data Rule = Rule { op :: Int -> Int, test :: Int, tNext :: Int, fNext :: Int }

parseMonkey :: [Text] -> (Array Int Rule, Array Int (Seq Int))
parseMonkey strs = (optimiseOp <$> rules, lists)
  where
    modLCM          = foldr lcm 1 (test <$> rules)
    optimiseOp rule = rule { op = (`mod` modLCM) . op rule }
    (rules, lists)  = bimap fromList fromList . unzip $ fmap parseOne strs
    parseOne str    = (rule, items)
      where
        [_, l1, l2, l3, l4, l5]     = snd . breakOn ":" <$> T.lines str
        items                       = L.fromList (readInt <$> T.splitOn "," l1)
        operator x                  = let trans "old" = x
                                          trans n     = readInt n
                                      in  case T.words $ snd (breakOn "=" l2) of
          [x, "+", y] -> trans x + trans y
          [x, "*", y] -> trans x * trans y
        [testMod, tBranch, fBranch] = readInt . last . T.words <$> [l3, l4, l5]
        rule                        = Rule operator testMod tBranch fBranch

simulate :: Int -> Bool -> (Array Int Rule, Array Int (Seq Int)) -> Int
simulate round hasRelief (rules, lists) = (\[a, b] -> a * b) . take 2
                                        . sortOn negate $ runST $ do
  listsST  <- A.thaw lists
  (lb, ub) <- getBounds listsST
  countST  <- A.newArray (lb, ub) 0
  replicateM_ round . forM_ [lb..ub] $ \ix -> do
    list <- listsST A.! ix
    let rule = rules ! ix
    forM_ list $ \e -> do
      let e'   = (if hasRelief then (`div` 3) else id) $ op rule e
      let next = if e' `mod` test rule == 0 then tNext rule else fNext rule
      A.adjust' listsST (L.|> e') next
    A.adjust' countST (+ length list) ix
    listsST A.=: ix $ L.empty
  getElems countST

day11Part1 :: (Array Int Rule, Array Int (Seq Int)) -> Int
day11Part1 = simulate 20 True

day11Part2 :: (Array Int Rule, Array Int (Seq Int)) -> Int
day11Part2 = simulate 10000 False

main :: IO ()
main = do
  monkey <- parseMonkey . readGroups <$> readInput "day11"
  print $ day11Part1 monkey
  print $ day11Part2 monkey
