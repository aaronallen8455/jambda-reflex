{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.JamState where

import            Control.Lens (makeLenses)
import            Data.IORef
import qualified  Data.IntMap as M

import            Jambda.Types.Semaphore (Semaphore)
import            Jambda.Types.Layer (Layer)
import            Jambda.Types.Newtypes (BPM, Vol)

data JamState =
  JamState
    { _jamStLayersRef      :: !(IORef (M.IntMap Layer))      -- ^ A reference to a Map of all Layers
    , _jamStTempoRef       :: !(IORef BPM)                   -- ^ Holds reference to the tempo
    , _jamStVolumeRef      :: !(IORef Vol)                   -- ^ Holds reference to the volume level
    , _jamStElapsedSamples :: !(IORef Double)                -- ^ Number of samples that have elapsed during playback
    , _jamStSemaphore      :: !(Semaphore)                   -- ^ Semaphore used to manage concurrency
    , _jamStStartPlayback  :: !(IO ())                       -- ^ Start playback
    , _jamStStopPlayback   :: !(IO ())                       -- ^ Stop playback
    }

makeLenses ''JamState
