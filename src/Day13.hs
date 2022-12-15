-- Question source: https://adventofcode.com/2022/day/13

import qualified Data.Set as S
import qualified Data.Text as T
import           Utilities

data Signal = Val Int | List [Signal]

instance Eq Signal where
  Val m   == Val n   = m == n
  Val m   == s2      = List [Val m] == s2
  s1      == Val n   = s1 == List [Val n]
  List xs == List ys = length xs == length ys && all (uncurry (==)) (zip xs ys)

instance Ord Signal where
  Val m         <= Val n   = m <= n
  Val m         <= s2      = List [Val m] <= s2
  s1            <= Val n   = s1 <= List [Val n]
  List []       <= List _  = True
  List _        <= List [] = False
  List (x : xs) <= List (y : ys)
    | x == y    = List xs <= List ys
    | otherwise = x < y

mkSignals :: String -> (Signal, Signal)
mkSignals str = let (signal1, "") = work str1; (signal2, "") = work str2
                    (str1, '\n' : str2) = break (== '\n') str
                in  (signal1, signal2)
  where
    work str = case span (`elem` ['0'..'9']) str of
      ("", _)  -> listSignal str
      (n, rem) -> (Val $ read n, rem)
    listSignal str  = let ("[", inner)    = splitAt 1 str
                          (signals, rest) = manySignals inner
                          ("]", rem)      = splitAt 1 rest
                      in  (List signals, rem)
    manySignals str = case head str of
      ',' -> manySignals (tail str)
      ']' -> ([], str)
      _   -> let (signal, rest) = work str; (signals, rem) = manySignals rest
             in  (signal : signals, rem)

day13Part1 :: [(Signal, Signal)] -> Int
day13Part1 = sum . map fst . filter snd . zip [1..] . map (uncurry (<))

day13Part2 :: [(Signal, Signal)] -> Int
day13Part2 signals = (1 + S.findIndex (List [Val 2]) sigSet)
                   * (1 + S.findIndex (List [Val 6]) sigSet)
  where
    sigSet = S.union (S.fromList (concatMap (uncurry ((. pure) . (:))) signals))
                     (S.fromList [List [Val 2], List [Val 6]])

main :: IO ()
main = do
  input <- fmap (mkSignals . T.unpack) . readGroups <$> readInput "day13"
  print $ day13Part1 input
  print $ day13Part2 input
