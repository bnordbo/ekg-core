{-# LANGUAGE BangPatterns #-}
-- | This module defines a type for mutable, integer-valued counters.
-- Counters are non-negative, monotonically increasing values and can
-- be used to track e.g. the number of requests served since program
-- start.  All operations on counters are thread-safe.
module System.Remote.Counter
    (
      Counter
    , inc
    , add
    ) where

import Control.Monad.IO.Class
import Data.IORef (atomicModifyIORef)
import System.Remote.Counter.Internal

-- | Increase the counter by one.
inc :: MonadIO m => Counter -> m ()
inc (C ref) = liftIO $! do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n + 1 in (n', n')
    return ()

-- | Increase the counter by the given amount.
add :: MonadIO m => Counter -> Int -> m ()
add (C ref) i = liftIO $! do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n + i in (n', n')
    return ()
