{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Question source: https://adventofcode.com/2022/day/21

import           Control.Monad
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Ratio
import           Data.Text (Text)
import qualified Data.Text as T
import           Gadgets.Memoise
import           Utilities

data Monkey = Num Int | Op (Ratio Int -> Ratio Int -> Ratio Int) Text Text

mkMonkey :: [Text] -> Map Text Monkey
mkMonkey = M.fromList . fmap worker
  where
    worker str = case readIntMaybe expr of
      Just n  -> (name, Num n)
      Nothing -> (name, Op (getOp op) m1 m2)
      where
        [m1, op, m2] = T.words expr
        (name, expr) = breakOn ": " str
        getOp "+"    = (+)
        getOp "-"    = (-)
        getOp "*"    = (*)
        getOp "/"    = (/)

memoiseFunc :: Map Text Monkey -> MemoisedFunc (Map Text (Ratio Int))
memoiseFunc mMap = fromRec $ \m -> case mMap M.! m of
  Num n       -> pure $ n % 1
  Op op m1 m2 -> liftM2 op (apply m1) (apply m2)

day21Part1 :: Map Text Monkey -> Int
day21Part1 = numerator . flip evalMemoise (apply "root") . memoiseFunc

day21Part2 :: Map Text Monkey -> Int
day21Part2 mMap = numerator $ (y0 - x0) / (y1 - y0 + x1 - x0)
  where
    Op _ m1 m2 = mMap M.! "root"
    evalWith n = evalMemoise (memoiseFunc (M.insert "humn" (Num n) mMap))
               $ liftM2 (,) (apply m1) (apply m2)
    (x0, y0)   = evalWith 0
    (x1, y1)   = evalWith 1

main :: IO ()
main = do
  input <- mkMonkey . T.lines <$> readInput "day21"
  print $ day21Part1 input
  print $ day21Part2 input
