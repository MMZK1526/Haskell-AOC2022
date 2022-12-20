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

data Blueprint = BP { oreBP :: Int,        clayBP  :: Int
                    , obsBP :: (Int, Int), geodeBP :: (Int, Int) }

data WorkState = WS { stuff     :: Stuff
                    , machines  :: Machines
                    , blueprint :: Blueprint
                    , blacklist :: [StuffType]
                    , curMax    :: Int }

mkBlueprint :: Text -> Blueprint
mkBlueprint str = BP (ints !! 1) (ints !! 2) (ints !! 3, ints !! 4) (ints !! 5, ints !! 6)
  where
    ints = catMaybes $ readIntMaybe <$> T.words str

getNexts :: Int -> Blueprint -> [StuffType] -> (Machines, Stuff) -> [([StuffType], (Machines, Stuff))]
getNexts 1 _ _ (m, s)              = [([], (m, s))]
getNexts time BP {..} bList (m, s) = zip (repeat []) (snd <$> moves) ++ [(map fst moves, (m, s))]
  where
    moves = filter (\(t, _) -> t `notElem` bList) $ geodeChoice ++ obsChoice ++ clayChoice ++ oreChoice
    oreChoice = do
      guard $ s !!! Ore + (time - 4) * (m !!! Ore - maximum [oreBP, clayBP, fst obsBP, fst geodeBP]) < 0
      guard $ s !!! Ore >= oreBP
      return (Ore, (adjust (+ 1) Ore m, adjust (+ (-oreBP)) Ore s))
    clayChoice = do
      guard $ s !!! Clay + (time - 3) * (m !!! Clay - snd obsBP) < 0
      guard $ s !!! Ore >= clayBP
      return (Clay, (adjust (+ 1) Clay m, adjust (+ (-clayBP)) Ore s))
    obsChoice = do
      guard $ s !!! Obs + (time - 2) * (m !!! Obs - snd geodeBP) < 0
      guard $ s !!! Clay >= snd obsBP && s !!! Ore >= fst obsBP
      return (Obs, (adjust (+ 1) Obs m, adjust (+ (-snd obsBP)) Clay $ adjust (+ (-fst obsBP)) Ore s))
    geodeChoice = do
      guard $ s !!! Obs >= snd geodeBP && s !!! Ore >= fst geodeBP
      return (Geode, (adjust (+ 1) Geode m, adjust (+ (-snd geodeBP)) Obs $ adjust (+ (-fst geodeBP)) Ore s))

produce :: Machines -> Stuff -> Stuff
produce mac =
  flip (foldr (\o -> adjust (+ mac !!! o) o)) [Ore, Clay, Obs, Geode]

nextSteps :: Int -> State WorkState [([StuffType], (Machines, Stuff))]
nextSteps time = do
  ws <- get
  let cur@(m, s) = (machines ws, stuff ws)
  let time'      = if snd (geodeBP $ blueprint ws) > s !!! Obs then time - 1 else time
  curBest <- gets curMax
  if 2 * (s !!! Geode + time * m !!! Geode) + time' * time' < 2 * curBest + time'
    then pure []
    else do
      let nexts = getNexts time (blueprint ws) (blacklist ws) cur
      return $ if length nexts == 5 then init nexts else nexts

simulation :: Int -> Blueprint -> Int
simulation time bp = curMax $ execState (worker time) initWS
  where
    initWS = WS (A.fromList [0, 0, 0, 0]) (A.fromList [1, 0, 0, 0]) bp [] 0
    worker n
      | n == 0 = do
        result <- (!!! Geode) <$> gets stuff
        modify' (\ws -> ws { curMax = max (curMax ws) result })
      | otherwise     = do
        ws   <- get
        let cur@(m, _) = (machines ws, stuff ws)
        next <- nextSteps n
        forM_ next $ \(bList, (m', s')) -> do
          modify (\ws -> ws { blacklist = bList, machines = m', stuff = produce m s' })
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
