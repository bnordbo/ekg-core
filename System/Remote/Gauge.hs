{-# LANGUAGE BangPatterns #-}
-- | This module defines a type for mutable, integer-valued gauges.
-- Gauges are variable values and can be used to track e.g. the
-- current number of concurrent connections. All operations on gauges
-- are thread-safe.
module System.Remote.Gauge
    (
      Gauge
    , inc
    , dec
    , add
    , subtract
    , set
    , modify
    ) where

import Control.Monad.IO.Class
import Data.IORef (atomicModifyIORef)
import Prelude hiding (subtract)
import System.Remote.Gauge.Internal

-- | Increase the gauge by one.
inc :: MonadIO m => Gauge -> m ()
inc (C ref) = liftIO $! do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n + 1 in (n', n')
    return ()

-- | Decrease the gauge by one.
dec :: MonadIO m => Gauge -> m ()
dec (C ref) = liftIO $! do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n - 1 in (n', n')
    return ()

-- | Increase the gauge by the given amount.
add :: MonadIO m => Gauge -> Int -> m ()
add (C ref) i = liftIO $! do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n + i in (n', n')
    return ()

-- | Decrease the gauge by the given amount.
subtract :: MonadIO m => Gauge -> Int -> m ()
subtract (C ref) i = liftIO $! do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n - i in (n', n')
    return ()

-- | Set the gauge to the given value.
set :: MonadIO m => Gauge -> Int -> m ()
set (C ref) !i = liftIO $! atomicModifyIORef ref $ \ _ -> (i, ())

-- | Set the gauge to the result of applying the given function to the
-- value.
modify :: MonadIO m => (Int -> Int) -> Gauge -> m ()
modify f (C ref) = liftIO $! do
    !_ <- atomicModifyIORef ref $ \ i -> let i' = f i in (i', i')
    return ()
