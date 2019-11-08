{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.Persistence
  ( BeatFileName(..)
  , PersistedBeat(..)
  ) where

import           Data.Aeson.TH
import qualified Data.IntMap as M
import qualified Data.Text as T

import           Jambda.Types.Newtypes (BPM)
import           Jambda.Types.UI (PersistedLayerUI)

newtype BeatFileName =
  BeatFileName { getBeatFileName :: T.Text }
  deriving (Eq, Ord, Show, Read)

data PersistedBeat =
  PersistedBeat
    { persistedTempo :: !BPM
    , persistedLayers :: !(M.IntMap PersistedLayerUI)
    }

$(deriveJSON defaultOptions ''PersistedBeat)
