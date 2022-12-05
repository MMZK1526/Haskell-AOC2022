{-# LANGUAGE OverloadedStrings #-}

-- Question source: https://adventofcode.com/2022/day/5

import           Control.Monad
import           Control.Monad.ST
import           Data.Array (Array)
import           Data.Array.ST
import           Data.Bifunctor
import           Data.Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as L
import           Data.Text (Text)
import qualified Data.Text as T
import           Gadgets.Array.Mutable
import qualified Gadgets.Array.ST as A
import           Utilities

type CrateConfig     = Array Int (Seq Char)
type CrateConfigST s = STArray s Int (Seq Char)

parseCrate :: Text -> (CrateConfig, [(Int, Int, Int)])
parseCrate str = (crates, (\[a, b, c] -> (a, b, c)) <$> instrList)
  where
    (crateStr, instrStr) = breakOn "\n\n" str
    instrList            = map (mapMaybe readIntMaybe . T.splitOn " ")
                               (T.lines instrStr)
    cratePositions       = getCargoNames <$> init (T.lines crateStr)
    getCargoNames        = ( map snd . filter ((== 1) . (`mod` 4) . fst)
                           . zip [0..] ) . T.unpack
    numColumns           = length $ head cratePositions
    crates               = runST $ do
      cratesST <- A.newArray (1, numColumns) L.empty
      forM_ cratePositions $ \row -> forM_ (zip [1..] row) $ \(ix, cargo) ->
        when (cargo /= ' ') (void $ adjust' cratesST (L.|> cargo) ix)
      A.unsafeFreeze cratesST

parseSegment :: Text -> ((Int, Int), (Int, Int))
parseSegment = let worker = bimap readInt readInt . breakOn "-"
               in  bimap worker worker . breakOn ","

simulateCrate :: Bool -> (CrateConfig, [(Int, Int, Int)]) -> String
simulateCrate keepOrder (craits, instrs) = runST $ do
  cratesST <- A.thaw craits
  forM_ instrs $ \(count, from, to) -> do
    fromColumn <- cratesST ! from
    toColumn   <- cratesST ! to
    let (moving, rest) = L.splitAt count fromColumn
    cratesST =: from $ rest
    cratesST =: to $ (if keepOrder then id else L.reverse) moving L.>< toColumn
  columns <- getElems cratesST
  return $ (`L.index` 0) <$> columns

day5Part1 :: (CrateConfig, [(Int, Int, Int)]) -> String
day5Part1 = simulateCrate False

day5Part2 :: (CrateConfig, [(Int, Int, Int)]) -> String
day5Part2 = simulateCrate True

main :: IO ()
main = do
  input <- parseCrate <$> readInput "day5"
  putStrLn $ day5Part1 input
  putStrLn $ day5Part2 input
