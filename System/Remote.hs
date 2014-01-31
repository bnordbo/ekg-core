module System.Remote
    ( -- * Types
      C.Counters
    , C.Gauges
    , C.Labels
    , C.Monitor
    , Section (..)

    , C.newMonitor
      -- * User-defined counters, gauges, and labels
    , C.getCounter
    , C.getGauge
    , C.getLabel

      -- * Sampling
    , C.Number (..)
    , C.Metric (..)
    , C.Metrics
    , C.sampleAll

    , C.buildAll
    , C.buildCombined
    , buildMany
    , buildOne
    ) where

import Data.Aeson.Types (Value)
import Control.Monad.IO.Class (MonadIO)

import qualified Data.ByteString      as S
import qualified Data.Text            as T
import qualified System.Remote.Common as C

-- | Selector type for denoting parts of a 'Monitor' value in
-- @buildMany@ and @buildOne@.
data Section
    = SecCounters
    | SecGauges
    | SecLabels
    deriving (Eq, Ord, Show)

-- | Turn counters, gauges or labels into JSON.
buildMany :: MonadIO m => Section -> C.Monitor -> m Value
buildMany SecCounters mon = C.buildMany (C.userCounters mon)
buildMany SecGauges   mon = C.buildMany (C.userGauges mon)
buildMany SecLabels   mon = C.buildMany (C.userLabels mon)

-- | Turn a single counter, gauge or label into JSON.
buildOne :: MonadIO m => T.Text -> Section -> C.Monitor -> m (Maybe S.ByteString)
buildOne name SecCounters mon = C.buildOne (C.userCounters mon) name
buildOne name SecGauges   mon = C.buildOne (C.userGauges mon) name
buildOne name SecLabels   mon = C.buildOne (C.userLabels mon) name
