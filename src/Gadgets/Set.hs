{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Gadgets.Set where

import           Data.Set (Set)
import qualified Data.Set as S

pattern Empty :: Set a
pattern Empty <- (S.null -> True)
  where
    Empty = S.empty

infixr 5 :<|
pattern (:<|) :: Ord a => a -> Set a -> Set a
pattern a :<| as <- (S.minView -> Just (a, as))
  where
    a :<| as = S.insert a as

infixl 5 :|>
pattern (:|>) :: Ord a => Set a -> a -> Set a
pattern as :|> a <- (S.maxView -> Just (a, as))
  where
    as :|> a = S.insert a as
