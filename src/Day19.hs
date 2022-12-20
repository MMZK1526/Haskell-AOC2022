{-# LANGUAGE RecordWildCards #-}

-- Question source: https://adventofcode.com/2022/day/19

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Array
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Gadgets.Array as A
import           Utilities

data StuffType = Ore | Clay | Obs | Geode deriving (Enum, Eq)

type Stuff    = Array Int Int
type Machines = Array Int Int

(!!!) :: Array Int a -> StuffType -> a
(!!!) = (. fromEnum) . (!)

adjust :: (a -> a) -> StuffType -> Array Int a -> Array Int a
adjust = (. fromEnum) . flip . flip A.adjust'

data Blueprint = BP { oreBP  :: Int,        clayBP  :: Int
                    , obsBP  :: (Int, Int), geodeBP :: (Int, Int)
                    , oreReq :: Int }

data WorkState = WS { stuff     :: Stuff
                    , machines  :: Machines
                    , blueprint :: Blueprint
                    , blacklist :: [StuffType]
                    , curMax    :: Int }

mkBlueprint :: Text -> Blueprint
mkBlueprint str 
  = let ints = catMaybes $ readIntMaybe <$> T.words str
    in  BP (ints !! 1) (ints !! 2) (ints !! 3, ints !! 4) (ints !! 5, ints !! 6)
           (maximum [ints !! 1, ints !! 2, ints !! 3, ints !! 5])

getNexts :: Int -> Blueprint -> [StuffType] -> (Machines, Stuff) -> [([StuffType], (Machines, Stuff))]
getNexts 1 _ _ (m, s)              = [([], (m, s))]
getNexts time BP {..} bList (m, s) = zip (repeat []) moves ++ [(bList', (m, s))]
  where
    (bList', moves) = unzip $ makeOre ++ makeClay ++ makeGeode ++ makeObs
    makeOre         = do
      guard $ s !!! Ore + (time - 1) * (m !!! Ore - oreReq) < 0
           && s !!! Ore >= oreBP && Ore `notElem` bList
      return (Ore, (adjust (+ 1) Ore m, adjust (+ (-oreBP)) Ore s))
    makeClay        = do
      guard $ s !!! Clay + (time - 3) * (m !!! Clay - snd obsBP) < 0
           && s !!! Ore >= clayBP && Clay `notElem` bList
      return (Clay, (adjust (+ 1) Clay m, adjust (+ (-clayBP)) Ore s))
    makeObs         = do
      guard $ s !!! Obs + (time - 2) * (m !!! Obs - snd geodeBP) < 0
           && s !!! Clay >= snd obsBP && s !!! Ore >= fst obsBP
           && Obs `notElem` bList
      return (Obs, (adjust (+ 1) Obs m, adjust (+ (-snd obsBP)) Clay $ adjust (+ (-fst obsBP)) Ore s))
    makeGeode       = do
      guard $ s !!! Obs >= snd geodeBP && s !!! Ore >= fst geodeBP
           && Geode `notElem` bList
      return (Geode, (adjust (+ 1) Geode m, adjust (+ (-snd geodeBP)) Obs $ adjust (+ (-fst geodeBP)) Ore s))

nextSteps :: Int -> State WorkState [([StuffType], (Machines, Stuff))]
nextSteps time = do
  ws@WS { blueprint = bp } <- get
  let cur@(m, s) = (machines ws, stuff ws)
  let worker n i
        | n >= snd (geodeBP bp) = 0
        | otherwise             = 1 + worker (n + m !!! Obs + i) (i + 1)
  let time'      = time - worker (s !!! Obs) 0
  curBest <- gets curMax
  return $ if 2 * (s !!! Geode + time * m !!! Geode)
            + time' * time' <= 2 * curBest + time'
    then []
    else getNexts time (blueprint ws) (blacklist ws) cur

simulation :: Int -> Blueprint -> Int
simulation time bp = curMax $ execState (worker time) initWS
  where
    initWS = WS (A.fromList [0, 0, 0, 0]) (A.fromList [1, 0, 0, 0]) bp [] 0
    worker n
      | n == 0    = do
        result <- (!!! Geode) <$> gets stuff
        modify' (\ws -> ws { curMax = max (curMax ws) result })
      | otherwise = do
        ws   <- get
        let cur@(m, _) = (machines ws, stuff ws)
        next <- nextSteps n
        forM_ next $ \(bList, (m', s')) -> do
          modify (\ws -> ws { blacklist = bList, machines = m'
                            , stuff     = foldr (\o -> adjust (+ m !!! o) o) 
                                                s' [Ore, Clay, Obs, Geode] })
          worker (n - 1)

day18Part1 :: [Blueprint] -> Int
day18Part1 = sumWorker (\(ix, c) -> ix * simulation 24 c) . zip [1..]

day18Part2 :: [Blueprint] -> Int
day18Part2 = product . map (simulation 32)

main :: IO ()
main = do
  input <- map mkBlueprint . T.lines <$> readInput "day19"
  print $ day18Part1 input
  print . day18Part2 $ take 3 input
