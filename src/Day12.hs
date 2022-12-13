-- Question source: https://adventofcode.com/2022/day/12

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Array
import           Data.List
import qualified Data.Set as S
import qualified Data.Text as T
import           Gadgets.Array
import           Utilities

floodfill :: (Char -> Char -> Bool) -> Bool -> (Char -> Bool)
          -> Array (Int, Int) Char -> Int
floodfill legalSteps startWithE endCondition hillMap
  = evalState (go (1, [startLoc])) (S.singleton startLoc)
  where
    bds           = bounds hillMap
    getNbs (r, c) = filter ( \pos' -> inRange bds pos'
                          && legalSteps (hillMap !@ (r, c)) (hillMap !@ pos') )
                           [(r, c + 1), (r, c - 1), (r + 1, c), (r - 1, c)]
    arr !@ ix     = case arr ! ix of
      'S' -> 'a'
      'E' -> 'z'
      pos -> pos
    ixPairs       = assocs hillMap
    Just sLoc     = fst <$> find ((== 'S') . snd) ixPairs
    Just eLoc     = fst <$> find ((== 'E') . snd) ixPairs
    startLoc      = if startWithE then eLoc else sLoc
    go (i, frs)   = do
        frs' <- fmap concat . forM frs $ \fr -> do
          visited <- get
          forM (filter (`notElem` visited) (getNbs fr)) $ \pos -> do
            visited <- modify' (S.insert pos) >> get
            pure pos
        visited <- get
        if any (endCondition . (hillMap !)) frs'
          then pure i
          else go (i + 1, frs')

day12Part1 :: Array (Int, Int) Char -> Int
day12Part1 = floodfill ((>=) . succ) False (== 'E')

day12Part2 :: Array (Int, Int) Char -> Int
day12Part2 = floodfill ((. succ) . (<=)) True (`elem` "Sa")

main :: IO ()
main = do
  hillMap <- from2DListR . fmap T.unpack . T.lines <$> readInput "day12"
  print $ day12Part1 hillMap
  print $ day12Part2 hillMap
