{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Jambda.Data.Layer
  ( newLayer
  , readChunk
  , getSamples
  , syncLayer
  , resetLayer
  , applyLayerBeatChange
  , applyLayerOffsetChange
  , applyLayerSourceChange
  , applyLayerVolChange
  , applyLayerPanChange
  ) where

import           Control.Lens hiding ((:>))
import           Data.Stream.Infinite (Stream(..))
import qualified Data.Stream.Infinite as Stream
import           Data.IORef (readIORef, modifyIORef')
import qualified Data.IntMap as M
import           Data.List.NonEmpty (NonEmpty)

import           Jambda.Data.Constants (taperLength)
import           Jambda.Data.Conversions (numSamplesForCellValue, numSamplesToCellValue)
import           Jambda.Data.Stream (dovetail, linearTaper, silence)
import           Jambda.Types

-- | Create a new layer with the given Pitch using defaults
-- for all other fields
newLayer :: SoundSource -> Layer
newLayer soundSource =
  Layer
    { _layerBeat         = pure ( Cell 1 Nothing )
    , _layerParsedCode   = [ Cell 1 Nothing ]
    , _layerCellOffset   = 0
    , _layerCellPrefix   = 0
    , _layerSamplePrefix = []
    , _layerSoundSource  = soundSource
    , _layerVol          = 1
    , _layerPan          = 0
    }

-- | Progress a layer by the given number of samples
-- returning the resulting samples and the modified layer.
readChunk :: Int -> BPM -> Layer -> (Layer, [Sample])
readChunk bufferSize bpm layer@Layer{..}
  -- Only read from the prefix if it is not fully consumed
  | prefixValue >= cellToTake =
    ( layer & layerSamplePrefix %~ drop bufferSize
            & layerCellPrefix   -~ cellToTake
    , Stream.take bufferSize $ _layerSamplePrefix `Stream.prepend` silence
    )
  -- if prefix is fully consumed, `getSamples` provides the rest
  | otherwise = ( remLayer, take bufferSize $ samples )
  where
    cellToTake  = numSamplesToCellValue bpm $ fromIntegral bufferSize
    prefixValue = layer^.layerCellPrefix

    (numPrefixSamples, remPrefixCell) = numSamplesForCellValue bpm prefixValue
    prefixSamples =
      Stream.take numPrefixSamples $ _layerSamplePrefix `Stream.prepend` silence
    (remLayer, newSamples) =
      getSamples bpm
                 ( layer & layerBeat %~ onHead ( fmap ( + remPrefixCell ) ) )
                 ( bufferSize - numPrefixSamples )
                 ( drop numPrefixSamples _layerSamplePrefix )
    samples = prefixSamples ++ newSamples

-- | Pull the specified number of samples from a layer.
-- returns the samples and the modified layer.
getSamples :: BPM -> Layer -> Int -> [Sample] -> (Layer, [Sample])
getSamples bpm layer nsamps prevSource
  | nsamps <= wholeCellSamps = (newLayer', take nsamps source)
  | otherwise = _2 %~ ( take wholeCellSamps source ++ )
              $ getSamples
                  bpm
                  ( newLayer' & layerBeat %~ onHead ( fmap ( + leftover ) ) )
                  ( nsamps - wholeCellSamps )
                  ( drop wholeCellSamps source )
  where
    ( c :> cells ) = layer^.layerBeat
    source = case maybe (layer^.layerSoundSource) id ( c^.cellSource ) of
               SSPitch pitch -> linearTaper taperLength
                              $ dovetail ( pitchToFreq pitch ) prevSource
               SSWav wav -> wav^.wavSamples

    ( wholeCellSamps, leftover ) = numSamplesForCellValue bpm ( c^.cellValue )
    newCellPrefix = c^.cellValue
                  - numSamplesToCellValue bpm ( fromIntegral nsamps )
                  + leftover
    newLayer' = layer & layerBeat         .~ cells
                      & layerCellPrefix   .~ newCellPrefix
                      & layerSamplePrefix .~ (drop nsamps source)

-- | Modifies the entire layer map, syncing to the elasped samples.
modifyLayers :: JamState
             -> (M.IntMap Layer -> M.IntMap Layer)
             -> IO ()
modifyLayers st modifier = signalSemaphore ( st^.jamStSemaphore ) $ do
  elapsedSamples <- readIORef ( st^.jamStElapsedSamples )
  tempo <- readIORef ( st^.jamStTempoRef )

  let elapsedCells = numSamplesToCellValue tempo elapsedSamples

  modifyIORef' ( st^.jamStLayersRef ) $ \layers ->
    syncLayer elapsedCells <$> modifier layers

-- | Modify the layer at a specific index. Syncs all layers with the current elapsed samples.
modifyLayer :: JamState
            -> Int
            -> (Layer -> Layer)
            -> IO ()
modifyLayer st i modifier = modifyLayers st ( ix i %~ modifier )

-- | Changes the beat code for all the layers simultaneously
applyLayerBeatChange :: JamState
                     -> M.IntMap (NonEmpty Cell')
                     -> IO ()
applyLayerBeatChange st allParsedCells = do
  let updateLayer _ newCells = Just
                             . ( layerBeat .~ Stream.cycle newCells )
                             . ( layerParsedCode .~ newCells )

  modifyLayers st $
    M.mergeWithKey updateLayer
                   ( const mempty )
                   ( const mempty )
                   allParsedCells

-- | Apply the current contents of the offset field of a layer
applyLayerOffsetChange :: JamState -> Int -> CellValue -> IO ()
applyLayerOffsetChange st i cellVal =
  modifyLayer st i ( layerCellOffset .~ cellVal )

-- | Apply the contents of the source field to the layer
applyLayerSourceChange :: JamState -> Int -> SoundSource -> IO ()
applyLayerSourceChange st i src =
  modifyIORef' ( st^.jamStLayersRef ) $
    ix i . layerSoundSource .~ src

applyLayerVolChange :: JamState -> Int -> Float -> IO ()
applyLayerVolChange st i vol =
  modifyIORef' ( st^.jamStLayersRef ) $
    ix i . layerVol .~ vol'
  where vol' = min 1 $ max 0 vol

applyLayerPanChange :: JamState -> Int -> Float -> IO ()
applyLayerPanChange st i pan =
  modifyIORef' ( st^.jamStLayersRef ) $
    ix i . layerPan .~ pan'
      where pan' = max (-1) $ min 1 pan

-- | Fast-forward a layer to the current time position
syncLayer :: CellValue -> Layer -> Layer
syncLayer elapsedCells layer
  | remainingElapsed <= 0 =
      layer & layerCellPrefix .~ abs remainingElapsed
  | otherwise =
      layer & layerBeat       .~ newCells
            & layerCellPrefix .~ cellPrefix
  where
    remainingElapsed       = elapsedCells - layer^.layerCellOffset
    cycleSize              = sum $ layer^.layerParsedCode^..traverse.cellValue
    elapsedCycles          = remainingElapsed / cycleSize
    wholeCycles            = fromIntegral @ Integer $ truncate elapsedCycles
    cellsToDrop            = remainingElapsed - wholeCycles * cycleSize
    cellCycle              = Stream.cycle $ layer^.layerParsedCode
    (cellPrefix, newCells) = dropCells cellsToDrop cellCycle
    dropCells !dc ( c :> cs )
      | c^.cellValue >= dc = ( c^.cellValue - dc, cs )
      | otherwise = dropCells ( dc - c^.cellValue ) cs

-- | Reset a layer to it's initial state
resetLayer :: Layer -> Layer
resetLayer layer =
  layer & layerBeat         .~ ( Stream.cycle $ layer^.layerParsedCode )
        & layerCellPrefix   .~ ( layer^.layerCellOffset )
        & layerSamplePrefix .~ []

onHead :: (a -> a) -> Stream a -> Stream a
onHead f ( x :> xs ) = f x :> xs
