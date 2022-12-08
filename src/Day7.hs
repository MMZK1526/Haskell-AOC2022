{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Question source: https://adventofcode.com/2022/day/7

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Utilities

data Directory = Dir { name :: String
                     , mFolder :: Maybe (Map String Directory)
                     , size :: Int }

mkDirectory :: [String] -> Directory
mkDirectory = work [Dir "/" (Just M.empty) 0]
  where
    emptyDir           = flip (`Dir` Just M.empty) 0
    putSubDir dir l    = case words l of
      ["dir", fName]   -> dir
        { mFolder = M.insertWith (const id) fName (emptyDir fName)
                <$> mFolder dir }
      [sizeStr, fName] -> dir
        { mFolder = M.insert fName (Dir fName Nothing (read sizeStr))
                <$> mFolder dir
        , size    = size dir + read sizeStr }
    merge child parent = parent
      { mFolder = M.insert (name child) child <$> mFolder parent
      , size    = size parent + size child }
    work dirs []       = foldl1 merge dirs
    work dirs (x : xs) = case words (drop 2 x) of
      ["cd", path] -> case path of
        ".." -> let child : parent : dirs' = dirs
                in  work (merge child parent : dirs') xs
        "/"  -> work [foldl1 merge dirs] xs
        _    -> work ((fromJust (mFolder (head dirs)) M.! path) : dirs) xs
      ["ls"]       -> let cur : dirs' = dirs
                          (ls, xs')   = span ((/= '$') . head) xs
                      in  work (foldl' putSubDir cur ls : dirs') xs'

day7Part1 :: Directory -> Int
day7Part1 Dir {..} = case mFolder of
  Nothing     -> 0
  Just folder -> sum (day7Part1 <$> M.elems folder)
               + if size <= 100000 then size else 0

day7Part2 :: Directory -> Int
day7Part2 dir = minimum . filter (>= size dir - 40000000) $ getSizes dir
  where
    getSizes dir = case mFolder dir of
      Nothing     -> []
      Just folder -> size dir
                   : (filter (isJust . mFolder) (M.elems folder) >>= getSizes)

main :: IO ()
main = do
  input <- mkDirectory . fmap T.unpack . T.lines <$> readInput "day7"
  print $ day7Part1 input
  print $ day7Part2 input
