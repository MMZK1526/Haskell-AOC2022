{-# LANGUAGE RecordWildCards #-}

-- Question source: https://adventofcode.com/2022/day/19

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Utilities

data StuffType = Ore | Clay | Obs | Geode deriving (Show, Eq, Ord)

type Stuff    = Map StuffType Int
type Machines = Map StuffType Int

data Blueprint = BP { oreBP :: Int,        clayBP  :: Int
                    , obsBP :: (Int, Int), geodeBP :: (Int, Int) } deriving (Show, Eq, Ord)

data WorkState = WS { stuff     :: Stuff
                    , machines  :: Machines
                    , blueprint :: Blueprint
                    , blacklist :: [StuffType]
                    , curMax    :: Int
                    , cache     :: Map Int [(Machines, Stuff)] } deriving (Show, Eq, Ord)

mkBlueprint :: Text -> Blueprint
mkBlueprint str = BP (ints !! 1) (ints !! 2) (ints !! 3, ints !! 4) (ints !! 5, ints !! 6)
  where
    ints = catMaybes $ readIntMaybe <$> T.words str

getNexts :: Blueprint -> [StuffType] -> (Machines, Stuff) -> [([StuffType], (Machines, Stuff))]
getNexts BP {..} bList (m, s) = zip (repeat []) (snd <$> moves) ++ [(map fst moves, (m, s))]
  where
    moves = filter (\(t, (m, s)) -> t `notElem` bList && all (>= 0) s && m M.! Ore <= 4)
      [ (Geode, (M.adjust (+ 1) Geode m, M.adjust (+ (-snd geodeBP)) Obs $ M.adjust (+ (-fst geodeBP)) Ore s))
      , (Obs, (M.adjust (+ 1) Obs m, M.adjust (+ (-snd obsBP)) Clay $ M.adjust (+ (-fst obsBP)) Ore s))
      , (Clay, (M.adjust (+ 1) Clay m, M.adjust (+ (-clayBP)) Ore s))
      , (Ore, (M.adjust (+ 1) Ore m, M.adjust (+ (-oreBP)) Ore s)) ]

inferior :: (Machines, Stuff) -> (Machines, Stuff) -> Bool
inferior (m1, s1) (m2, s2) 
  = and (zipWith (<=) (snd <$> M.toAscList m1) (snd <$> M.toAscList m2))
 && and (zipWith (<=) (snd <$> M.toAscList s1) (snd <$> M.toAscList s2))

produce :: Machines -> Stuff -> Stuff
produce mac =
  flip (foldr (\o -> M.adjust (+ mac M.! o) o)) [Ore, Clay, Obs, Geode]

nextSteps :: Int -> State WorkState [([StuffType], (Machines, Stuff))]
nextSteps time = do
  ws <- get
  let bp         = blueprint ws
  let cur@(m, s) = (machines ws, stuff ws)
  let curCache   = cache ws M.! time
  curBest <- gets curMax
  if 2 * (s M.! Geode + time * m M.! Geode) + time * time < 2 * curBest + time || any (inferior cur) curCache
    then pure []
    else do
      modify (\ws -> ws { cache = M.insert time (cur : curCache) (cache ws) })
      let nexts = getNexts bp (blacklist ws) cur
      return $ if length nexts == 5 then init nexts else nexts

decentTryout :: Int -> Blueprint -> Int
decentTryout time bp@BP {..} = curMax $ execState (worker time) initWS
  where
    initWS = WS (M.fromList [(Ore, 0), (Clay, 0), (Obs, 0), (Geode, 0)])
                   (M.fromList [(Ore, 1), (Clay, 0), (Obs, 0), (Geode, 0)])
                   bp [] (-1) (M.fromList . zip [1..time] $ repeat [])
    worker n
      | n == 0 = do
        result <- (M.! Geode) <$> gets stuff
        modify' (\ws -> ws { curMax = max (curMax ws) result })
      | otherwise     = do
        ws   <- get
        let cur@(m, s) = (machines ws, stuff ws)
        let moves = filter (\(t, (m, s)) -> all (>= 0) s && m M.! Ore <= 4)
              [ (Geode, (M.adjust (+ 1) Geode m, M.adjust (+ (-snd geodeBP)) Obs $ M.adjust (+ (-fst geodeBP)) Ore s))
              , (Obs, (M.adjust (+ 1) Obs m, M.adjust (+ (-snd obsBP)) Clay $ M.adjust (+ (-fst obsBP)) Ore s))
              , (Clay, (M.adjust (+ 1) Clay m, M.adjust (+ (-clayBP)) Ore s))
              , (Ore, (M.adjust (+ 1) Ore m, M.adjust (+ (-oreBP)) Ore s)) ]
        let (m', s') = case lookup Geode moves of
              Just n  -> n
              Nothing -> case lookup Obs moves of
                Just n  -> n
                Nothing -> fromMaybe (m, s) $ if snd obsBP * m M.! Ore <= fst obsBP * m M.! Clay
                  then if m M.! Ore == 4 then Nothing else lookup Ore moves
                  else lookup Clay moves
        modify (\ws -> ws { machines = m', stuff = produce m s' })
        worker (n - 1)

simulation :: Int -> Blueprint -> Int
simulation time bp = curMax $ execState (worker time) initWS
  where
    initWS = WS (M.fromList [(Ore, 0), (Clay, 0), (Obs, 0), (Geode, 0)])
                   (M.fromList [(Ore, 1), (Clay, 0), (Obs, 0), (Geode, 0)])
                   bp [] (decentTryout time bp) (M.fromList . zip [1..time] $ repeat [])
    worker n
      | n == 0 = do
        result <- (M.! Geode) <$> gets stuff
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
