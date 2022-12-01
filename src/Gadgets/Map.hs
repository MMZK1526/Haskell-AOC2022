{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Gadgets.Map where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Tuple (swap)

pattern Empty :: Map k a
pattern Empty <- (M.null -> True)
  where
    Empty = M.empty

swapkv :: Ord a => Map k a -> Map a k
swapkv = M.fromList . map swap . M.assocs

infixr 5 :<|
pattern (:<|) :: Ord k => (k, a) -> Map k a -> Map k a
pattern a :<| as <- (M.minViewWithKey -> Just (a, as))
  where
    a :<| as = uncurry M.insert a as

infixl 5 :|>
pattern (:|>) :: Ord k => Map k a -> (k, a) -> Map k a
pattern as :|> a <- (M.maxViewWithKey -> Just (a, as))
  where
    as :|> a = uncurry M.insert a as
