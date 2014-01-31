{-# LANGUAGE BangPatterns #-}
-- | This module defines a type for mutable, string-valued labels.
-- Labels are variable values and can be used to track e.g. the
-- command line arguments or other free-form values. All operations on
-- labels are thread-safe.
module System.Remote.Label
    (
      Label
    , set
    , modify
    ) where

import Control.Monad.IO.Class
import Data.IORef (atomicModifyIORef)
import qualified Data.Text as T
import System.Remote.Label.Internal

-- | Set the label to the given value.
set :: MonadIO m => Label -> T.Text -> m ()
set (C ref) !i = liftIO $! atomicModifyIORef ref $ \ _ -> (i, ())

-- | Set the label to the result of applying the given function to the
-- value.
modify :: MonadIO m => (T.Text -> T.Text) -> Label -> m ()
modify f (C ref) = liftIO $! do
    !_ <- atomicModifyIORef ref $ \ i -> let i' = f i in (i', i')
    return ()
