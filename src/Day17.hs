{-# LANGUAGE FlexibleContexts #-}

-- Question source: https://adventofcode.com/2022/day/17

import           Control.Monad
import           Control.Monad.ST.Lazy
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Array.ST
import           Data.Bifunctor
import           Data.InfList (InfList(..))
import qualified Data.InfList as I
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import           Gadgets.Array.Mutable as A
import qualified Gadgets.Array.ST.Lazy as A
import           Utilities

data Shape = Shape { bottom :: (Int, Int), offsets :: [(Int, Int)] }

type SimulationState = ( Int, InfList (Int, Int -> Shape), InfList (Int, Bool)
                       , Map (Int, Int) (Int, Int) )

r1, r2, r3, r4, r5 :: Int -> Shape
r1 h = Shape (0, h) [(-1, 0), (1, 0), (2, 0)]
r2 h = Shape (0, h) [(0, 1), (0, 2), (-1, 1), (1, 1)]
r3 h = Shape (0, h) [(-1, 0), (1, 0), (1, 1), (1, 2)]
r4 h = Shape (-1, h) [(0, 1), (0, 2), (0, 3)]
r5 h = Shape (0, h) [(-1, 0), (0, 1), (-1, 1)]

allOffsets :: Shape -> [(Int, Int)]
allOffsets Shape { bottom = b@(x, y), offsets = ofts }
  = b : fmap (bimap (x +) (y +)) ofts

shift :: Bool -> Shape -> Shape
shift True shape@Shape { bottom = (x, y) }  = shape { bottom = (x + 1, y) }
shift False shape@Shape { bottom = (x, y) } = shape { bottom = (x - 1, y) }

down :: Shape -> Shape
down shape@Shape { bottom = (x, y) } = shape { bottom = (x, y - 1) }

simulateOne :: Int -> STArray s Int IntSet -> StateT SimulationState (ST s) Int
simulateOne ct boardST = do
  (h, (ix, shape) ::: shapes, dirs, cache) <- get
  (h', dirs')                              <- fall (shape (h + 4)) dirs
  put (max h h', shapes, dirs', M.insert (ix, fst $ I.head dirs) (ct, h) cache)
  return $ ct + 1
  where
    isIn (x, y)
      | x < -3 || x > 3 = pure False
      | otherwise       = IS.notMember y <$> (boardST ! x)
    fall shape ((_, dir) ::: dirs) = do
      let xOfts   = fst <$> allOffsets shape
      let onSide  = all (inRange (-3, 3) . (if dir then succ else pred)) xOfts
      moveable <- fmap and . lift $ forM (allOffsets (shift dir shape)) isIn
      let shape'  = if moveable then shift dir shape else shape
      let shape'' = down shape'
      moveable <- fmap and . lift $ forM (allOffsets shape'') isIn
      if moveable
        then fall shape'' dirs
        else do
          lift . forM_ (allOffsets shape') $ \(x, y) ->
              adjust' boardST (IS.insert y) x
          return (snd (bottom shape') + maximum (snd <$> offsets shape'), dirs)

naiveSimulation :: Int -> InfList (Int, Bool) -> Int
naiveSimulation count dirs = runST $ do
  boardST <- A.newArray (-3, 3) $ IS.singleton 0
  (h, _, _, _) <- execStateT (replicateM count $ simulateOne 0 boardST)
                             ( 0, I.cycle $ zip [1..] [r1, r2, r3, r4, r5]
                             , dirs, M.empty)
  return h

simulation :: Int -> InfList (Int, Bool) -> Int
simulation count dirs = runST $ do
  boardST              <- A.newArray (-3, 3) $ IS.singleton 0
  let worker i = do
        (h, (sIx, _) ::: _, (dIx, _) ::: _, cache) <- get
        simulateOne i boardST
        case cache M.!? (sIx, dIx) of
          Nothing -> worker (i + 1)
          Just x  -> do
            modify (\(h, s, d, _) -> (h, s, d,  M.singleton (sIx, dIx) (i, h)))
            (x :::) <$> worker (i + 1)
  ((ni, oi), (nh, oh)) <- takeArith <$> evalStateT (worker 1) init
  return $ if count < oh
    then naiveSimulation count dirs
    else let (repeatCount, remainder) = quotRem (count - oh) (ni - oi)
         in  repeatCount * (nh - oh) + naiveSimulation (oh + remainder) dirs
  where
    takeArith ((a1, b1) ::: n2@(a2, b2) ::: n3@(a3, b3) ::: ns)
      | a3 - a2 == a2 - a1 && b3 - b2 == b2 - b1 = ((a2, a1), (b2, b1))
      | otherwise                                = takeArith $ n2 ::: n3 ::: ns
    init = (0, I.cycle $ zip [1..] [r1, r2, r3, r4, r5], dirs, M.empty)

day17Part1 :: InfList (Int, Bool) -> Int
day17Part1 = naiveSimulation 2022

day17Part2 :: InfList (Int, Bool) -> Int
day17Part2 = simulation 1000000000000

main :: IO ()
main = do
  input <- I.cycle . zip [1..] . map (== '>') . T.unpack <$> readInput "day17"
  print $ day17Part1 input
  print $ day17Part2 input
