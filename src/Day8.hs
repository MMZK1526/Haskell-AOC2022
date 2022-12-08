-- Question source: https://adventofcode.com/2022/day/8

import           Data.Array
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import           Gadgets.Array
import           Utilities

day8Part1 :: Array (Int, Int) Char -> Int
day8Part1 treeArr = length . filter id
                  $ zipWith (||) (fst <$> elems visArr1) (fst <$> elems visArr2)
  where
    range   = bounds treeArr
    visArr1 = tabulate range (go visArr1 (-1))
    visArr2 = tabulate range (go visArr2 1)
    go arr step (i, j)
      | not (inRange range (i + step, j)) = (True, (cur, cur))
      | not (inRange range (i, j + step)) = (True, (cur, cur))
      | otherwise                         = ( visH (<) || visV (<)
                                            , (visH max, visV max) )
      where
        cur     = treeArr ! (i, j)
        visV op = snd (snd (arr ! (i + step, j))) `op` cur
        visH op = fst (snd (arr ! (i, j + step))) `op` cur

day8Part2 :: Array (Int, Int) Char -> Int
day8Part2 treeArr = maximum 
                  $ zipWith (*) (elems $ uncurry (*) . fst <$> visArr1)
                                (elems $ uncurry (*) . fst <$> visArr2)
  where
    range   = bounds treeArr
    visArr1 = tabulate range (go visArr1 (-1))
    visArr2 = tabulate range (go visArr2 1)
    go arr step (i, j)
      | not (inRange range (i + step, j)) = ((0, 0), (M.empty, edgeMap i))
      | not (inRange range (i, j + step)) = ((0, 0), (edgeMap j, M.empty))
      | otherwise                         = (dists, ( M.insert cur j hMap
                                                    , M.insert cur i vMap ))
      where
        cur     = treeArr ! (i, j)
        dists   = (abs $ j - getLim hMap, abs $ i - getLim vMap)
        hMap    = fst $ snd (arr ! (i, j + step))
        vMap    = snd $ snd (arr ! (i + step, j))
        edgeMap = M.singleton ':' -- Larger than '9'
        getLim  = (if step < 0 then maximum else minimum)
                . snd . M.split (pred cur)

main :: IO ()
main = do
  input <- from2DListR . fmap T.unpack . T.lines <$> readInput "day8"
  print $ day8Part1 input
  print $ day8Part2 input
