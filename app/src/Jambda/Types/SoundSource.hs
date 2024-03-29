{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Jambda.Types.SoundSource where

import           Data.Aeson.TH
import           Control.Lens
import           Data.Function (on)

import           Jambda.Types.SoundSource.Pitch (Pitch)
import           Jambda.Types.SoundSource.Wav (Wav(..), PersistedWav)

data SoundSourceBase wav
  = SSPitch Pitch
  | SSWav wav

type SoundSource = SoundSourceBase Wav
type PersistedSoundSource = SoundSourceBase PersistedWav

instance Eq SoundSource where
  (SSPitch p1) == (SSPitch p2) = p1 == p2
  (SSPitch _) == _ = False
  _ == (SSPitch _) = False
  (SSWav w1) == (SSWav w2) = ( (==) `on` _wavLabel ) w1 w2

instance Ord SoundSource where
  compare (SSPitch p1) (SSPitch p2) = compare p1 p2
  compare (SSPitch _) _ = LT
  compare _ (SSPitch _) = GT
  compare (SSWav w1) (SSWav w2) = ( compare `on` _wavIdx ) w1 w2

makePrisms ''SoundSourceBase
$(deriveJSON defaultOptions ''SoundSourceBase)
