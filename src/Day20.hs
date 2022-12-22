-- Question source: https://adventofcode.com/2022/day/20

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.State
import           Data.Array.ST
import           Data.Sequence (Seq)
import qualified Data.Sequence as L
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Gadgets.Array.ST as A
import           Utilities

(!) :: Seq a -> Int -> a
xs ! ix = xs `L.index` (ix `mod` length xs)

simulation :: Int -> Seq Int -> Seq Int
simulation i xs = fst $ iterate simulateOne (xs, ixSeq) !! i
  where
    ixSeq                 = L.fromList [0..length xs - 1]
    simulateOne (xs, xis) = (xs', xis')
      where
        ixs         = L.fromList $ runST $ do
          ixsST <- A.newArray_ (0, length xs - 1)
          forM_ (L.zip ixSeq xis) $ \(ix, pos) -> writeArray ixsST pos ix
          getElems ixsST
        xis'        = (xis `L.index`) . (xis `L.index`) . fst . snd <$> result
        xs'         = fst <$> result
        (_, result) = execState (forM_ ixs worker) 
                                ((S.empty, S.empty), L.zip xs $ L.zip ixs ixSeq)
    worker posRaw         = do
      ((outSet, inSet), xifs) <- get
      let oftBack        = length . fst $ S.split posRaw outSet
      let oftFore        = length . fst $ S.split (posRaw, 0) inSet
      let pos            = posRaw - oftBack + oftFore
      let (val, (ix, _)) = xifs `L.index` pos
      let pos'           = (pos + val) `mod` (length xs - 1)
      let (xifs', flag)
            = ( L.insertAt pos' (val, (ix, flag)) $ L.deleteAt pos xifs
              , snd . snd $ xifs' `L.index` (pos' + 1) )
      put ((S.insert posRaw outSet, S.insert (flag - 1, posRaw) inSet), xifs')

day20Part1 :: Seq Int -> Int
day20Part1 xs = sumWorker (result !) [ixOf0 + 1000, ixOf0 + 2000, ixOf0 + 3000]
  where
    Just ixOf0 = L.findIndexL (== 0) result
    result     = worker 0 . L.zip xs . L.fromList $ replicate (length xs) False
    worker l xfs
      | l == length xs = fst <$> xfs
      | otherwise      = case xfs `L.index` l of
        (_, True)  -> worker (l + 1) xfs
        (x, False) -> worker l (move x)
      where
        move x = L.insertAt ((l + x) `mod` (length xs - 1)) (x, True)
               $ L.deleteAt l xfs

day20Part2 :: Seq Int -> Int
day20Part2 xs = sumWorker (result !) [ixOf0 + 1000, ixOf0 + 2000, ixOf0 + 3000]
  where
    result     = simulation 10 ((* 811589153) <$> xs)
    Just ixOf0 = L.findIndexL (== 0) result

main :: IO ()
main = do
  input <- L.fromList . map readInt . T.lines <$> readInput "day20"
  print $ day20Part1 input
  print $ day20Part2 input
