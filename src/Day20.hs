-- Question source: https://adventofcode.com/2022/day/20

import           Data.Sequence (Seq)
import qualified Data.Sequence as L
import           Data.Text (Text)
import qualified Data.Text as T
import           Utilities

(!) :: Seq a -> Int -> a
xs ! ix = xs `L.index` (ix `mod` length xs)

simulation :: Int -> Seq Int -> Seq Int
simulation i xs
  = snd <$> iterate (worker 0) (L.zip (L.fromList [0..(length xs - 1)]) xs) !! i
  where
    worker l ixs
      | l == length xs = ixs
      | otherwise      = worker (l + 1) (move (snd $ ixs `L.index` ix))
      where
        Just ix = L.findIndexL ((== l) . fst) ixs
        move x  = L.insertAt ((ix + x) `mod` (length xs - 1)) (l, x)
                $ L.deleteAt ix ixs

day20Part1 :: Seq Int -> Int
day20Part1 xs = sumWorker (result !) [ixOf0 + 1000, ixOf0 + 2000, ixOf0 + 3000]
  where
    Just ixOf0 = L.findIndexL (== 0) result
    result     = worker 0 $ L.zip xs (L.fromList $ replicate (length xs) False)
    worker l xfs
      | l == length xs = fst <$> xfs
      | otherwise      = case xfs `L.index` l of
        (_, True)  -> worker (l + 1) xfs
        (x, False) -> worker l (move x)
      where
        move x = L.insertAt ((l + x) `mod` (length xs - 1)) (x, True)
               $ L.deleteAt l xfs

day20Part2 :: Seq Int -> Int
day20Part2 xs
  = sumWorker (result !) [ixOf0 + 1000, ixOf0 + 2000, ixOf0 + 3000]
  where
    result     = simulation 10 ((* 811589153) <$> xs)
    Just ixOf0 = L.findIndexL (== 0) result

main :: IO ()
main = do
  input <- L.fromList . map readInt . T.lines <$> readInput "day20"
  print $ day20Part1 input
  print $ day20Part2 input
