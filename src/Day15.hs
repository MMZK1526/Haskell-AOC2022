{-# LANGUAGE OverloadedStrings #-}

-- Question source: https://adventofcode.com/2022/day/15

import           Control.Monad
import           Data.Bifunctor
import           Data.Ix
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Tuple
import           Utilities

mkBeacon :: Text -> ((Int, Int), (Int, Int))
mkBeacon str = let worker = bimap (readInt . T.drop 2) (readInt . T.drop 2)
                          . breakOn ", " . snd . breakOn "at "
               in  bimap worker worker $ breakOn ": " str

getDist :: (Int, Int) -> (Int, Int) -> (Int, Int)
getDist (sx, sy) (bx, by) = (abs (sx - bx), abs (sy - by))

getInter :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
getInter l1@((x11, y11), (x12, y12)) l2@((x21, y21), (x22, y22)) = do
  let (a, b, c, d) = (y12 - y11, x11 - x12, y22 - y21, x21 - x22)
  let (e, f)       = (x11 * y12 - y11 * x12, x21 * y22 - y21 * x22)
  let det          = a * d - b * c
  let s@(x, y)     = ((d * e - b * f) `div` det, (a * f - c * e) `div` det)
  guard $ a * x + b * y == e && c * x + d * y == f
  guard $ any (\l -> inRange l s || inRange (swap l) s) [l1, l2]
  return s

day15Part1 :: [((Int, Int), (Int, Int))] -> Int
day15Part1 sensors = sumWorker (uncurry (-) . swap) $ merge noSensorPoses []
  where
    noSensorPoses            = sort $ mapMaybe (uncurry worker) sensors
    merge [] acc             = acc
    merge (i@(ix, iy) : is) (h@(hx, hy) : hs)
      | ix <= hy  = merge is ((hx, max iy hy) : hs)
      | otherwise = merge is (i : h : hs)
    merge (i : is) []        = merge is [i]
    worker (sx, sy) (bx, by) = (sx - dx, sx + dx) <$ guard (dx >= 0)
      where
        dx = uncurry (+) (getDist (sx, sy) (bx, by)) - abs (sy - 2000000)

day15Part2 :: [((Int, Int), (Int, Int))] -> Int
day15Part2 sensors = uncurry ((+) . (* ub)) . head . filter verify
                   $ points ++ [(0, 0), (0, ub), (ub, 0), (ub, ub)]
  where
    verify pos      = all (\(sPos, d) -> uncurry (+) (getDist pos sPos) >= d)
                          (zip (fst <$> sensors) dists)
    (ub, isLegal)   = (4000000, inRange ((0, 0), (ub, ub)))
    dists           = succ . uncurry (+) . uncurry getDist <$> sensors
    expand (x, y) d = ( [((x - d, y), (x, y - d)), ((x + d, y), (x, y + d))]
                      , [((x, y - d), (x + d, y)), ((x - d, y), (x, y + d))] )
    points          = S.toList . S.fromList . filter isLegal . join
                    . uncurry (liftM2 getInter) . bimap concat concat
                    . unzip . zipWith expand (fst <$> sensors) $ dists

main :: IO ()
main = do
  input <- map mkBeacon . T.lines <$> readInput "day15"
  print $ day15Part1 input
  print $ day15Part2 input
