module Gadgets.Array where

import           Control.Monad (ap)
import           Data.Array 
  (Array, Ix, array, bounds, inRange, listArray, range, (!), (//))

-- | Making an array from a list, indexed from 0.
fromList :: [a] -> Array Int a
fromList xs = listArray (0, length xs - 1) xs

-- | Making a row-major 2D array from a list, indexed from (0, 0).
-- Will err on non-rectangular inputs.
from2DListR :: [[a]] -> Array (Int, Int) a
from2DListR xz = array ((0, 0), (length xz - 1, length (head xz) - 1)) $ concat
               $ zipWith (flip zipWith [0..] . (((,) .) . (,))) [0..] xz

-- | Making a column-major 2D array from a list, indexed from (0, 0).
-- Will err on non-rectangular inputs.
from2DListC :: [[a]] -> Array (Int, Int) a
from2DListC xz = array ((0, 0), (length (head xz) - 1, length xz - 1)) $ concat
               $ zipWith (flip zipWith [0..] . flip (((,) .) . (,))) [0..] xz

-- | Adjusts a value in the array with the given function.
-- It will do nothing if the index is out of bound.
adjust :: Ix i => Array i a -> (a -> a) -> i -> Array i a
adjust arr f i = arr // [(i, f $ arr ! i)]

-- | Strict version of "adjust".
adjust' :: Ix i => Array i a -> (a -> a) -> i -> Array i a
adjust' = (. ap seq) . adjust

-- | Constructs an array from a function.
{-# INLINE tabulate #-}
tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate (u, v) f = array (u, v) [ (i, f i) | i <- range (u, v)]

-- | Strict version of "tabulate".
{-# INLINE tabulate' #-}
tabulate' :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate' = (. ap seq) . tabulate

-- | Safe array access.
infixr 4 !?
(!?) :: Ix i => Array i a -> i -> Maybe a
arr !? i 
  | inRange (bounds arr) i = Just $ arr ! i
  | otherwise              = Nothing
