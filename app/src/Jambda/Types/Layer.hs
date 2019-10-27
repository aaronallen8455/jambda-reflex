{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jambda.Types.Layer where

import            Control.Lens hiding ((:>))
import            Data.List.NonEmpty (NonEmpty)
import            Data.Stream.Infinite (Stream(..))

import            Jambda.Types.Newtypes (CellValue, Sample)
import            Jambda.Types.SoundSource (SoundSource)
import            Jambda.Types.Cell (Cell')

data Layer =
  Layer
    { _layerBeat         :: !(Stream Cell')     -- ^ Infinite list of cells
    , _layerParsedCode   :: !(NonEmpty Cell')   -- ^ The parsed beatcode, finite list
    , _layerCellOffset   :: !CellValue          -- ^ Initial delay before first note
    , _layerCellPrefix   :: !CellValue          -- ^ Cell value before next note
    , _layerSamplePrefix :: ![Sample]           -- ^ Tail of a partially played source
    , _layerSoundSource  :: !SoundSource        -- ^ The sound representation to use
    , _layerVol          :: Float               -- ^ Volume coefficient of the layer
    , _layerPan          :: Float               -- ^ Pan of the layer (-1 to 1)
    , _layerMuted        :: Bool                -- ^ True if muted
    , _layerSoloed       :: Bool                -- ^ True if part of the soloed group
    }

makeLenses ''Layer

