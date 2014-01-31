{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module System.Remote.Common
    ( -- * Types
      Counters
    , Gauges
    , Labels
    , Monitor (..)

    , newMonitor

      -- * User-defined counters, gauges, and labels
    , getCounter
    , getGauge
    , getLabel

      -- * Sampling
    , Number(..)
    , Metric(..)
    , Metrics(..)
    , sampleAll
    , sampleCombined
    , sampleCounters
    , sampleGauges
    , sampleLabels

    , buildMany
    , buildAll
    , buildCombined
    , buildOne
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson.Types ((.=), Value)
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.HashMap.Strict as M
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified GHC.Stats as Stats
import Prelude hiding (read)

import System.Remote.Counter (Counter)
import qualified System.Remote.Counter.Internal as Counter
import System.Remote.Gauge (Gauge)
import qualified System.Remote.Gauge.Internal as Gauge
import System.Remote.Label (Label)
import qualified System.Remote.Label.Internal as Label

------------------------------------------------------------------------

-- Map of user-defined counters.
type Counters = M.HashMap T.Text Counter

-- Map of user-defined gauges.
type Gauges = M.HashMap T.Text Gauge

-- Map of user-defined labels.
type Labels = M.HashMap T.Text Label

-- | The set of 'Counters', 'Gauges' and 'Labels'.
data Monitor = Monitor
    { userCounters :: !(IORef Counters)
    , userGauges   :: !(IORef Gauges)
    , userLabels   :: !(IORef Labels)
    }

------------------------------------------------------------------------
-- * User-defined counters, gauges and labels

class Ref r t | r -> t where
    new :: IO r
    read :: r -> IO t

instance Ref Counter Int where
    new = Counter.new
    read = Counter.read

instance Ref Gauge Int where
    new = Gauge.new
    read = Gauge.read

instance Ref Label T.Text where
    new = Label.new
    read = Label.read

-- | Create a new 'Monitor' value with empty 'Gauges', 'Counters' and
-- 'Labels'.
newMonitor :: MonadIO m => m Monitor
newMonitor = liftIO $! Monitor
    <$> newIORef M.empty
    <*> newIORef M.empty
    <*> newIORef M.empty

-- | Lookup a 'Ref' by name in the given map.  If no 'Ref' exists
-- under the given name, create a new one, insert it into the map and
-- return it.
getRef :: Ref r t
       => T.Text                      -- ^ 'Ref' name
       -> IORef (M.HashMap T.Text r)  -- ^ 'Monitor' holding the 'Ref'
       -> IO r
getRef name mapRef = do
    empty <- new
    ref <- atomicModifyIORef mapRef $ \ m ->
        case M.lookup name m of
            Nothing  -> let m' = M.insert name empty m
                        in (m', empty)
            Just ref -> (m, ref)
    return ref
{-# INLINABLE getRef #-}

-- | Return the counter associated with the given name and monitor.
-- Multiple calls to 'getCounter' with the same arguments will return
-- the same counter.  The first time 'getCounter' is called for a
-- given name and monitor, a new, zero-initialized counter will be
-- returned.
getCounter :: MonadIO m
           => T.Text   -- ^ Counter name
           -> Monitor  -- ^ 'Monitor' holding the 'Counter'
           -> m Counter
getCounter name mon = liftIO $! getRef name (userCounters mon)

-- | Return the gauge associated with the given name and monitor.
-- Multiple calls to 'getGauge' with the same arguments will return
-- the same gauge.  The first time 'getGauge' is called for a given
-- name and monitor, a new, zero-initialized gauge will be returned.
getGauge :: MonadIO m
         => T.Text   -- ^ Gauge name
         -> Monitor  -- ^ Monitor holding the 'Gauge'
         -> m Gauge
getGauge name mon = liftIO $! getRef name (userGauges mon)

-- | Return the label associated with the given name and monitor.
-- Multiple calls to 'getLabel' with the same arguments will return
-- the same label.  The first time 'getLabel' is called for a given
-- name and monitor, a new, empty label will be returned.
getLabel :: MonadIO m
         => T.Text   -- ^ Label name
         -> Monitor  -- ^ Monitor holding the 'Label'
         -> m Label
getLabel name mon = liftIO $! getRef name (userLabels mon)

------------------------------------------------------------------------
-- * Sampling

-- | A sample of some metrics.
data Metrics = Metrics
    { metricsCounters :: !(M.HashMap T.Text Number)
    , metricsGauges   :: !(M.HashMap T.Text Number)
    , metricsLabels   :: !(M.HashMap T.Text T.Text)
    }

-- | Sample all metrics.
sampleAll :: MonadIO m => Monitor -> m Metrics
sampleAll mon = liftIO $! do
    counters <- readAllRefs (userCounters mon)
    gauges <- readAllRefs (userGauges mon)
    labels <- readAllRefs (userLabels mon)
    (gcCounters, gcGauges) <- partitionGcStats <$> getGcStats
    let allCounters = (map (mapSnd (I . fromIntegral)) counters ++ gcCounters)
        allGauges   = (map (mapSnd (I . fromIntegral)) gauges ++ gcGauges)
    return $! Metrics
        (M.fromList allCounters)
        (M.fromList allGauges)
        (M.fromList labels)

-- | The kind of metrics that can be tracked.
data Metric = Counter !Number
            | Gauge !Number
            | Label !T.Text

sampleCombined :: MonadIO m => Monitor -> m (M.HashMap T.Text Metric)
sampleCombined mon = do
    metrics <- sampleAll mon
    -- This assumes that the same name wasn't used for two different
    -- metric types.
    return $! M.unions [M.map Counter (metricsCounters metrics),
                        M.map Gauge (metricsGauges metrics),
                        M.map Label (metricsLabels metrics)]

sampleCounters :: MonadIO m => Monitor -> m (M.HashMap T.Text Number)
sampleCounters mon = liftIO $! metricsCounters <$> sampleAll mon

sampleGauges :: MonadIO m => Monitor -> m (M.HashMap T.Text Number)
sampleGauges mon = liftIO $! metricsGauges <$> sampleAll mon

sampleLabels :: MonadIO m => Monitor -> m (M.HashMap T.Text T.Text)
sampleLabels mon = liftIO $! metricsLabels <$> sampleAll mon

-- | Apply a function to the second component of a pair and evaluate
-- the result to WHNF.
mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = let !fy = f y in (x, fy)

------------------------------------------------------------------------
-- * JSON serialization

-- | All the stats exported by the server (i.e. GC stats plus user
-- defined counters).
data Stats = Stats
    !Stats.GCStats          -- GC statistics
    ![(T.Text, Json)]       -- Counters
    ![(T.Text, Json)]       -- Gauges
    ![(T.Text, Json)]       -- Labels
    {-# UNPACK #-} !Double  -- Milliseconds since epoch

instance A.ToJSON Stats where
    toJSON (Stats gcStats counters gauges labels t) = A.object $
        [ "server_timestamp_millis" .= t
        , "counters"                .= Assocs (json gcCounters ++ counters)
        , "gauges"                  .= Assocs (json gcGauges ++ gauges)
        , "labels"                  .= Assocs labels
        ]
      where
        (gcCounters, gcGauges) = partitionGcStats gcStats
        json = map (\ (x, y) -> (x, Json y))

-- | 'Stats' encoded as a flattened JSON object.
newtype Combined = Combined Stats

instance A.ToJSON Combined where
    toJSON (Combined (Stats s@(Stats.GCStats {..}) counters gauges labels t)) =
        A.object $
        [ "server_timestamp_millis"  .= t
        , "bytes_allocated"          .= bytesAllocated
        , "num_gcs"                  .= numGcs
        , "max_bytes_used"           .= maxBytesUsed
        , "num_bytes_usage_samples"  .= numByteUsageSamples
        , "cumulative_bytes_used"    .= cumulativeBytesUsed
        , "bytes_copied"             .= bytesCopied
        , "current_bytes_used"       .= currentBytesUsed
        , "current_bytes_slop"       .= currentBytesSlop
        , "max_bytes_slop"           .= maxBytesSlop
        , "peak_megabytes_allocated" .= peakMegabytesAllocated
        , "mutator_cpu_seconds"      .= mutatorCpuSeconds
        , "mutator_wall_seconds"     .= mutatorWallSeconds
        , "gc_cpu_seconds"           .= gcCpuSeconds
        , "gc_wall_seconds"          .= gcWallSeconds
        , "cpu_seconds"              .= cpuSeconds
        , "wall_seconds"             .= wallSeconds
        , "par_tot_bytes_copied"     .= gcParTotBytesCopied s
        , "par_avg_bytes_copied"     .= gcParTotBytesCopied s
        , "par_max_bytes_copied"     .= parMaxBytesCopied
        ] ++
        map (uncurry (.=)) counters ++
        map (uncurry (.=)) gauges ++
        map (uncurry (.=)) labels

-- | A list of string keys and JSON-encodable values.  Used to render
-- a list of key-value pairs as a JSON object.
newtype Assocs = Assocs [(T.Text, Json)]

instance A.ToJSON Assocs where
    toJSON (Assocs xs) = A.object $ map (uncurry (.=)) xs

-- | A group of either counters or gauges.
data Group = Group
     ![(T.Text, Json)]
    {-# UNPACK #-} !Double  -- Milliseconds since epoch

instance A.ToJSON Group where
    toJSON (Group xs t) =
        A.object $ ("server_timestamp_millis" .= t) : map (uncurry (.=)) xs

------------------------------------------------------------------------

-- | Get a snapshot of all values.  Note that we're not guaranteed to
-- see a consistent snapshot of the whole map.
readAllRefs :: Ref r t => IORef (M.HashMap T.Text r) -> IO [(T.Text, t)]
readAllRefs mapRef = do
    m <- readIORef mapRef
    forM (M.toList m) $ \ (name, ref) -> do
        val <- read ref
        return (name, val)
{-# INLINABLE readAllRefs #-}

readAllRefsAsJson :: (Ref r t, A.ToJSON t) => IORef (M.HashMap T.Text r)
                  -> IO [(T.Text, Json)]
readAllRefsAsJson mapRef =
    map (\ (x, y) -> (x, Json y)) `fmap` readAllRefs mapRef
{-# INLINABLE readAllRefsAsJson #-}


-- Existential wrapper used for OO-style polymorphism.
data Json = forall a. A.ToJSON a => Json a

instance A.ToJSON Json where
    toJSON (Json x) = A.toJSON x

-- | Many metrics can be either integer or floating point values. This
-- is captured by the 'Number' data type.
data Number = I !Int64
            | D !Double

instance A.ToJSON Number where
    toJSON (I n) = A.toJSON n
    toJSON (D n) = A.toJSON n

-- | Partition GC statistics into counters and gauges.
partitionGcStats :: Stats.GCStats -> ([(T.Text, Number)], [(T.Text, Number)])
partitionGcStats s@(Stats.GCStats {..}) = (counters, gauges)
  where
    counters = [
          ("bytes_allocated"          , I bytesAllocated)
        , ("num_gcs"                  , I numGcs)
        , ("num_bytes_usage_samples"  , I numByteUsageSamples)
        , ("cumulative_bytes_used"    , I cumulativeBytesUsed)
        , ("bytes_copied"             , I bytesCopied)
        , ("mutator_cpu_seconds"      , D mutatorCpuSeconds)
        , ("mutator_wall_seconds"     , D mutatorWallSeconds)
        , ("gc_cpu_seconds"           , D gcCpuSeconds)
        , ("gc_wall_seconds"          , D gcWallSeconds)
        , ("cpu_seconds"              , D cpuSeconds)
        , ("wall_seconds"             , D wallSeconds)
        ]
    gauges = [
          ("max_bytes_used"           , I maxBytesUsed)
        , ("current_bytes_used"       , I currentBytesUsed)
        , ("current_bytes_slop"       , I currentBytesSlop)
        , ("max_bytes_slop"           , I maxBytesSlop)
        , ("peak_megabytes_allocated" , I peakMegabytesAllocated)
        , ("par_tot_bytes_copied"     , I (gcParTotBytesCopied s))
        , ("par_avg_bytes_copied"     , I (gcParTotBytesCopied s))
        , ("par_max_bytes_copied"     , I parMaxBytesCopied)
        ]

------------------------------------------------------------------------

-- | Serve a collection of counters or gauges, as a JSON object.
buildMany :: (MonadIO m, Ref r t, A.ToJSON t)
          => IORef (M.HashMap T.Text r)
          -> m Value
buildMany mapRef = liftIO $! do
    list <- readAllRefsAsJson mapRef
    time <- getTimeMillis
    return . A.toJSON $ Group list time
{-# INLINABLE buildMany #-}

-- | Turn all counter, gauges and labels, built-in or not, into a
-- nested JSON object.
buildAll :: MonadIO m => Monitor -> m Value
buildAll (Monitor counters gauges labels) = liftIO $! do
    gcStats     <- getGcStats
    counterList <- readAllRefsAsJson counters
    gaugeList   <- readAllRefsAsJson gauges
    labelList   <- readAllRefsAsJson labels
    time        <- getTimeMillis
    return . A.toJSON $
        Stats gcStats counterList gaugeList labelList time

-- | Turn all counter, gauges and labels, built-in or not, into a
-- flattened JSON object.
buildCombined :: MonadIO m => Monitor -> m Value
buildCombined (Monitor counters gauges labels) = liftIO $! do
    gcStats     <- getGcStats
    counterList <- readAllRefsAsJson counters
    gaugeList   <- readAllRefsAsJson gauges
    labelList   <- readAllRefsAsJson labels
    time        <- getTimeMillis
    return . A.toJSON . Combined $
        Stats gcStats counterList gaugeList labelList time

buildOne :: (MonadIO m, Ref r t, Show t)
         => IORef (M.HashMap T.Text r)
         -> T.Text
         -> m (Maybe S.ByteString)
buildOne refs name = liftIO $! do
    m <- readIORef refs
    case M.lookup name m of
        Just counter -> do
            val <- read counter
            return $ Just $ S8.pack $ show val
        Nothing ->
            -- Try built-in (e.g. GC) refs
            case Map.lookup name builtinCounters of
                Just f -> do
                    gcStats <- liftIO getGcStats
                    return $ Just $ S8.pack $ f gcStats
                Nothing -> return Nothing
{-# INLINABLE buildOne #-}

getGcStats :: IO Stats.GCStats
#if MIN_VERSION_base(4,6,0)
getGcStats = do
    enabled <- Stats.getGCStatsEnabled
    if enabled
        then Stats.getGCStats
        else return emptyGCStats

emptyGCStats :: Stats.GCStats
emptyGCStats = Stats.GCStats
    { bytesAllocated         = 0
    , numGcs                 = 0
    , maxBytesUsed           = 0
    , numByteUsageSamples    = 0
    , cumulativeBytesUsed    = 0
    , bytesCopied            = 0
    , currentBytesUsed       = 0
    , currentBytesSlop       = 0
    , maxBytesSlop           = 0
    , peakMegabytesAllocated = 0
    , mutatorCpuSeconds      = 0
    , mutatorWallSeconds     = 0
    , gcCpuSeconds           = 0
    , gcWallSeconds          = 0
    , cpuSeconds             = 0
    , wallSeconds            = 0
    , parTotBytesCopied      = 0
    , parMaxBytesCopied      = 0
    }
#else
getGcStats = Stats.getGCStats
#endif

-- | A list of all built-in (e.g. GC) counters, together with a
-- pretty-printing function for each.
builtinCounters :: Map.Map T.Text (Stats.GCStats -> String)
builtinCounters = Map.fromList [
      ("bytes_allocated"          , show . Stats.bytesAllocated)
    , ("num_gcs"                  , show . Stats.numGcs)
    , ("max_bytes_used"           , show . Stats.maxBytesUsed)
    , ("num_bytes_usage_samples"  , show . Stats.numByteUsageSamples)
    , ("cumulative_bytes_used"    , show . Stats.cumulativeBytesUsed)
    , ("bytes_copied"             , show . Stats.bytesCopied)
    , ("current_bytes_used"       , show . Stats.currentBytesUsed)
    , ("current_bytes_slop"       , show . Stats.currentBytesSlop)
    , ("max_bytes_slop"           , show . Stats.maxBytesSlop)
    , ("peak_megabytes_allocated" , show . Stats.peakMegabytesAllocated)
    , ("mutator_cpu_seconds"      , show . Stats.mutatorCpuSeconds)
    , ("mutator_wall_seconds"     , show . Stats.mutatorWallSeconds)
    , ("gc_cpu_seconds"           , show . Stats.gcCpuSeconds)
    , ("gc_wall_seconds"          , show . Stats.gcWallSeconds)
    , ("cpu_seconds"              , show . Stats.cpuSeconds)
    , ("wall_seconds"             , show . Stats.wallSeconds)
    , ("par_tot_bytes_copied"     , show . gcParTotBytesCopied)
    , ("par_avg_bytes_copied"     , show . gcParTotBytesCopied)
    , ("par_max_bytes_copied"     , show . Stats.parMaxBytesCopied)
    ]

------------------------------------------------------------------------
-- Utilities for working with timestamps

-- | Return the number of milliseconds since epoch.
getTimeMillis :: IO Double
getTimeMillis = (realToFrac . (* 1000)) `fmap` getPOSIXTime

-- | Helper to work around rename in GHC.Stats in base-4.6.
gcParTotBytesCopied :: Stats.GCStats -> Int64
#if MIN_VERSION_base(4,6,0)
gcParTotBytesCopied = Stats.parTotBytesCopied
#else
gcParTotBytesCopied = Stats.parAvgBytesCopied
#endif
