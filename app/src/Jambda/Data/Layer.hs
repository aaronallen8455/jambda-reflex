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
  ) where

import           Control.Lens hiding ((:>))
import           Data.Stream.Infinite (Stream(..))
import qualified Data.Stream.Infinite as Stream
import           Data.IORef (readIORef, modifyIORef')
import qualified Data.IntMap as M
import           Data.List.NonEmpty (NonEmpty)

import           Jambda.Data.Constants (taperLength)
import           Jambda.Data.Conversions (numSamplesForCellValue, numSamplesToCellValue)
import           Jambda.Data.Stream (dovetail, linearTaper, silence, sineWave)
import           Jambda.Types

-- | Create a new layer with the given Pitch using defaults
-- for all other fields
newLayer :: Pitch -> Layer
newLayer pitch = Layer
  { _layerSource = source
  , _layerBeat = pure ( Cell 1 Nothing )
  , _layerParsedCode = [ Cell 1 Nothing ]
  , _layerCellOffset = 0
  , _layerCellPrefix = 0
  , _layerSourcePrefix = []
  , _layerSourceType = pitch
  }
    where
      freq = pitchToFreq pitch
      source = linearTaper taperLength $ sineWave freq 0

-- | Progress a layer by the given number of samples
-- returning the resulting samples and the modified layer.
readChunk :: Int -> BPM -> Layer -> (Layer, [Sample])
readChunk bufferSize bpm layer@Layer{..}
  | prefixValue >= cellToTake =
    ( layer & layerSourcePrefix %~ (drop bufferSize)
            & layerCellPrefix   -~ cellToTake
    , Stream.take bufferSize $ _layerSourcePrefix `Stream.prepend` silence
    )
  | otherwise = ( remLayer, take bufferSize $ samples )
  where
    cellToTake  = numSamplesToCellValue bpm $ fromIntegral bufferSize
    prefixValue = layer^.layerCellPrefix

    (numPrefixSamples, remPrefixCell) = numSamplesForCellValue bpm prefixValue
    prefixSamples =
      Stream.take numPrefixSamples $ _layerSourcePrefix `Stream.prepend` silence
    (remLayer, newSamples) =
      getSamples bpm
                 ( layer & layerBeat %~ onHead ( fmap ( + remPrefixCell ) ) )
                 ( bufferSize - numPrefixSamples )
                 ( drop numPrefixSamples _layerSourcePrefix )
    samples = prefixSamples ++ newSamples

-- | Pull the specified number of samples from a layer.
-- returns the samples and the modified layer.
getSamples :: BPM -> Layer -> Int -> [Sample] -> (Layer, [Sample])
getSamples bpm layer nsamps prevSource
  | nsamps <= wholeCellSamps = (newLayer', take nsamps source)
  | otherwise = _2 %~ ( take wholeCellSamps source ++ )
              $ getSamples bpm
                           ( layer & layerBeat %~ onHead ( fmap ( + leftover ) ) )
                           ( nsamps - wholeCellSamps )
                           ( drop wholeCellSamps source )
  where
    ( c :> cells ) = layer^.layerBeat
    source = linearTaper taperLength $
      maybe ( dovetail ( pitchToFreq $ layer^.layerSourceType ) $ prevSource )
            id
            ( dovetail <$> ( pitchToFreq <$> c^.cellSource )
                       <*> Just prevSource
            )
    ( wholeCellSamps, leftover ) = numSamplesForCellValue bpm ( c^.cellValue )
    newCellPrefix = c^.cellValue
                  - numSamplesToCellValue bpm ( fromIntegral nsamps )
                  + leftover
    newLayer' = layer & layerBeat         .~ cells
                      & layerCellPrefix   .~ newCellPrefix
                      & layerSourcePrefix .~ (drop nsamps source)

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
-- returns true if beat code is valid.
applyLayerOffsetChange :: JamState
                       -> Int
                       -> CellValue
                       -> IO ()
applyLayerOffsetChange st i cellVal =
  modifyLayer st i ( layerCellOffset .~ cellVal )

-- | Apply the contents of the source field to the layer
-- returning true if valid
applyLayerSourceChange :: JamState
                       -> Int
                       -> Pitch
                       -> IO ()
applyLayerSourceChange st i pitch =
  modifyIORef' ( st^.jamStLayersRef ) $ \layers ->
    let mbLayer = modifySource pitch <$> layers ^? ix i
     in maybe layers ( \x -> layers & ix i .~ x ) mbLayer

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

-- | Change the sound source (Pitch) of the layer
modifySource :: Pitch -> Layer -> Layer
modifySource pitch layer = do
  let freq = pitchToFreq pitch
      wave = sineWave freq 0
      newSource = linearTaper taperLength wave

   in layer & layerSource     .~ newSource
            & layerSourceType .~ pitch

-- | Reset a layer to it's initial state
resetLayer :: Layer -> Layer
resetLayer layer =
  layer & layerBeat         .~ ( Stream.cycle $ layer^.layerParsedCode )
        & layerCellPrefix   .~ ( layer^.layerCellOffset )
        & layerSourcePrefix .~ []

onHead :: (a -> a) -> Stream a -> Stream a
onHead f ( x :> xs ) = f x :> xs
