{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Gadgets.Memoise where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.Map (Map)
import qualified Data.Map as M

-- | A type class that supports adding a value at a given index.
class Table t where
  type Key t
  type Value t

  -- | The empty table.
  emptyTable :: t

  -- | Read the cache at the given index.
  readTable :: t -> Key t -> Maybe (Value t)

  -- | Insert the value at the given index.
  writeTable :: t -> Key t -> Value t -> t

  -- | Clear the cache for the given index. The default implementation does
  -- nothing.
  clearTable :: t -> Key t -> t
  clearTable = const

instance Hashable k => Table (HashMap k v) where
  type Key (HashMap k v)   = k
  type Value (HashMap k v) = v

  emptyTable = HM.empty
  readTable  = (HM.!?)
  writeTable = flip (flip . HM.insert)
  clearTable = flip HM.delete

instance Ord k => Table (Map k v) where
  type Key (Map k v)   = k
  type Value (Map k v) = v

  emptyTable = M.empty
  readTable  = (M.!?)
  writeTable = flip (flip . M.insert)
  clearTable = flip M.delete

instance Eq k => Table [(k, v)] where
  type Key [(k, v)]   = k
  type Value [(k, v)] = v

  emptyTable        = []
  readTable         = flip lookup
  writeTable ts k v = fst $ go ts
    where
      go [] = ((k, v) : ts, True)
      go ((k', v') : ts)
        | k' == k   = ((k, v) : ts, False)
        | flag      = (rem, flag)
        | otherwise = ((k', v') : rem, flag)
        where
          (rem, flag) = go ts
  clearTable [] _   = []
  clearTable ((k', v') : ts) k
    | k' == k   = ts
    | otherwise = (k', v') : clearTable ts k

-- | A type that captures a "memoised" function under a monadic environment.
data MemoisedFuncT t m =
  MemoiseT { getFunc  :: Key t -> MemoisedT t m (Value t)
           , getTable :: t }

-- | A type that captures a "memoised" pure function.
type MemoisedFunc t = MemoisedFuncT t Identity

type Memoised t    = State (MemoisedFunc t)
type MemoisedT t m = StateT (MemoisedFuncT t m) m

-- | Create a memoised function from a pure one.
fromPure :: Monad m => Table t => (Key t -> Value t) -> MemoisedFuncT t m
fromPure f = MemoiseT (pure . f) emptyTable
{-# INLINE fromPure #-}

-- | Create a memoised function from a recursive one.
fromRec :: Monad m => Table t
        => (Key t -> MemoisedT t m (Value t)) -> MemoisedFuncT t m
fromRec f = MemoiseT f emptyTable
{-# INLINE fromRec #-}

evalMemoise :: MemoisedFunc t -> Memoised t a -> a
evalMemoise = flip evalState

evalMemoiseT :: Monad m => MemoisedFuncT t m -> MemoisedT t m a -> m a
evalMemoiseT = flip evalStateT

execMemoise :: MemoisedFunc t -> Memoised t a -> t
execMemoise = (getTable .) . flip execState

execMemoiseT :: Monad m => MemoisedFuncT t m -> MemoisedT t m a -> m t
execMemoiseT = (fmap getTable .) . flip execStateT

runMemoise :: MemoisedFunc t -> Memoised t a -> (a, t)
runMemoise = (second getTable .) . flip runState

runMemoiseT :: Monad m => MemoisedFuncT t m -> MemoisedT t m a -> m (a, t)
runMemoiseT = (fmap (second getTable) .) . flip runStateT

-- | Apply given argument to the memoised function within the "MemoisedT" state.
-- Either use the memoised cache or calculate and update the cache.
apply :: Monad m => Table t => Key t -> MemoisedT t m (Value t)
apply k = do
  func  <- gets getFunc
  table <- gets getTable
  case readTable table k of
    Just v  -> pure v
    Nothing -> do
      v <- func k
      modify (\mem -> mem { getTable = writeTable (getTable mem) k v })
      return v

-- | Similar to @apply@, but do not cache the result.
apply' :: Monad m => Table t => Key t -> MemoisedT t m (Value t)
apply' k = do
  table <- gets getTable
  case readTable table k of
    Just v  -> pure v
    Nothing -> gets getFunc >>= ($ k)

-- | Replace the memoised table.
setTable :: Monad m => Table t => t -> MemoisedT t m ()
setTable t = modify (\mem -> mem { getTable = t })
{-# INLINE setTable #-}

-- | Add a pair of (key, value) entry into the memoised table.
addEntry :: Monad m => Table t => Key t -> Value t -> MemoisedT t m ()
addEntry k v = modify (\mem -> mem { getTable = writeTable (getTable mem) k v })
{-# INLINE addEntry #-}

-- | Memoised fibonacci sequence.
fibMem :: Monad m => MemoisedFuncT [(Integer, Integer)] m
fibMem = fromRec $ \case
  0 -> pure 0
  1 -> pure 1
  n -> liftM2 (+) (apply (n - 1)) (apply (n - 2))

fibRangeExample :: Integer -> Integer -> [Integer]
fibRangeExample a b = evalMemoise fibMem $ mapM apply [a..b]
