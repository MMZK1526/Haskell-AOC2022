{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Question source: https://adventofcode.com/2022/day/10

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.Writer.CPS
import           Data.Array.ST
import           Data.Sequence (Seq)
import qualified Data.Sequence as L
import           Data.Text (Text)
import qualified Data.Text as T
import           Gadgets.Array.Mutable
import qualified Gadgets.Array.ST as A
import           Utilities

toInstrs :: [Text] -> [Maybe Int]
toInstrs = (Nothing :) . map (\str -> case breakOn " " str of
  ("noop", _) -> Nothing
  ("addx", n) -> Just $ readInt n)

simulation :: [Maybe Int] -> Seq Int
simulation = execWriter . foldM_ logOne 1
  where
    logOne n Nothing  = n <$ tell (L.singleton n)
    logOne n (Just d) = (n + d) <$ tell (L.fromList [n, n + d])

day10Part1 :: [Maybe Int] -> Int
day10Part1 instrs = sumWorker (\c -> c * result `L.index` (c - 1)) [20, 60..220]
  where
    result = simulation instrs

day10Part2 :: [Maybe Int] -> String
day10Part2 instrs = concatMap worker (zip [1..] screen)
  where
    worker (ix, b) = (if b then '\x2588' else ' ')
                   : if ix `mod` 40 == 0 then "\n" else []
    screen         = runST $ do
      screenST <- A.newArray ((0, 0), (5, 39)) False
      let result = simulation instrs
      forM_ [0..239] $ \c -> do
        let spritePos = result `L.index` c
        let (cy, cx)  = c `quotRem` 40
        when (abs (spritePos - cx) <= 1) (screenST =: c `quotRem` 40 $ True)
      getElems screenST

main :: IO ()
main = do
  instrs <- toInstrs . T.lines <$> readInput "day10"
  print $ day10Part1 instrs
  putStrLn $ day10Part2 instrs
