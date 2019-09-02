{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.JamState where

import           Control.Lens (makeLenses)
import           Data.IORef
import qualified Data.IntMap as M
import qualified Data.Vector as V

import           Jambda.Types.Layer (Layer)
import           Jambda.Types.Newtypes (BPM, Vol)
import           Jambda.Types.Semaphore (Semaphore)
import           Jambda.Types.SoundSource.Wav (Wav)

data JamState =
  JamState
    { _jamStLayersRef      :: !(IORef (M.IntMap Layer))      -- ^ A reference to a Map of all Layers
    , _jamStTempoRef       :: !(IORef BPM)                   -- ^ Holds reference to the tempo
    , _jamStVolumeRef      :: !(IORef Vol)                   -- ^ Holds reference to the volume level
    , _jamStElapsedSamples :: !(IORef Double)                -- ^ Number of samples that have elapsed during playback
    , _jamStSemaphore      :: !(Semaphore)                   -- ^ Semaphore used to manage concurrency
    , _jamStStartPlayback  :: !(IO ())                       -- ^ Start playback
    , _jamStStopPlayback   :: !(IO ())                       -- ^ Stop playback
    , _jamStWavSources     :: !(V.Vector Wav)                -- ^ All the .wav sources currently in memory
    , _jamStFinalizer      :: !(IO ())                       -- ^ Release allocated resources
    }

makeLenses ''JamState
