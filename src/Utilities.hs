{-# LANGUAGE OverloadedStrings  #-}

module Utilities where

import           Data.Either
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import           Gadgets.IO

sumWorker :: Traversable t => Num n => (a -> n) -> t a -> n
sumWorker = (sum .) . fmap

breakOn :: Text -> Text -> (Text, Text)
breakOn pattern str = let (str1, str2) = T.breakOn pattern str
                      in  (str1, T.drop (T.length pattern) str2)

readInput :: String -> IO Text
readInput path = 
  handleDNE (const $ T.readFile ("src/" ++ path ++ ".txt")) $ T.readFile path

readInt :: Integral a => Text -> a
readInt = fst . fromRight undefined . T.signed T.decimal . T.stripStart

readInts :: Integral a => Text -> [a]
readInts = map readInt . T.lines

readGroups :: Text -> [Text]
readGroups = T.splitOn "\n\n"
