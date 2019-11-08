{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.SoundSource.Pitch
  ( Pitch(..)
  , Note(..)
  , pitchText
  , pitchToFreq
  ) where

import           Data.Aeson.TH
import           Data.Bifunctor
import qualified Data.Text as T
import           System.Random

import           Jambda.Types.Newtypes (Freq(..), Octave(..))

data Pitch = Pitch Note Octave
  deriving (Eq, Ord)

instance Random Pitch where
  randomR (Pitch n1 o1, Pitch n2 o2) g =
    let ( octave, g' ) = randomR (o1, o2) g
        ( pitch, g'' ) = first toEnum $ randomR ( fromEnum n1, fromEnum n2 ) g'
     in ( Pitch pitch octave, g'' )
  random = randomR ( Pitch ANat 3, Pitch AFlat 5 )

data Note
  = ANat
  | BFlat
  | BNat
  | CNat
  | DFlat
  | DNat
  | EFlat
  | ENat
  | FNat
  | GFlat
  | GNat
  | AFlat
  deriving (Bounded, Enum, Eq, Ord)

pitchText :: Pitch -> T.Text
pitchText (Pitch n o) = noteText n <> T.pack ( show ( getOctave o ) )

noteText :: Note -> T.Text
noteText ANat  = "A"
noteText BFlat = "Bb"
noteText BNat  = "B"
noteText CNat  = "C"
noteText DFlat = "Db"
noteText DNat  = "D"
noteText EFlat = "Eb"
noteText ENat  = "E"
noteText FNat  = "F"
noteText GFlat = "Gb"
noteText GNat  = "G"
noteText AFlat = "Ab"

pitchToFreq :: Pitch -> Freq
pitchToFreq ( Pitch note octave ) = Freq $
    440 * 2 ** ( ( noteIndex + oct * 12 - 48 ) / 12 )
  where
    noteIndex = fromIntegral $ fromEnum note
    oct = fromIntegral $ getOctave octave

$(deriveJSON defaultOptions ''Note)
$(deriveJSON defaultOptions ''Pitch)
