{-# LANGUAGE GADTs #-}
{-# LANGUAGE Strict #-}
module Jambda.Data.Audio
  ( audioCallback
  , aggregateChunks
  ) where

import           Control.Arrow ((&&&), (***))
import           Control.Monad (join)
import           Data.List (transpose)
import qualified Data.IntMap as Map
import           Data.IORef
import qualified Data.Vector.Storable.Mutable as MV

import           Control.Lens
import qualified SDL

import           Jambda.Types
import           Jambda.Data.Layer (readChunk)

audioCallback :: Semaphore
              -> IORef ( Map.IntMap Layer )
              -> IORef BPM
              -> IORef Double
              -> IORef Vol
              -> SDL.AudioFormat actualSampleType
              -> MV.IOVector actualSampleType
              -> IO ()
audioCallback semaphore layersRef bpmRef elapsedSamplesRef volumeRef SDL.FloatingLEAudio vec = do
  waitSemaphore semaphore

  layers <- readIORef layersRef
  bpm <- readIORef bpmRef
  ( Vol vol ) <- readIORef volumeRef

  let numSamples = MV.length vec `div` 2
      chunkMap   = readChunk numSamples bpm <$> layers :: Map.IntMap (Layer, [Sample])
      lVol = uncurry $ volC L
      rVol = uncurry $ volC R
      samples    = uncurry zip . (lVol &&& rVol) <$> Map.elems chunkMap :: [[(Sample, Sample)]]
      newLayers  = fst <$> chunkMap :: Map.IntMap Layer
      combined   = join bimap ( ( * ( vol / 10 ) ) . getSample )
               <$> aggregateChunks samples

  iforM_ combined $ \i (l, r) -> do
    MV.write vec ( i * 2 ) l     -- Left channel
    MV.write vec ( i * 2 + 1 ) r -- Right channel

  writeIORef layersRef newLayers
  modifyIORef' elapsedSamplesRef ( + fromIntegral numSamples )

audioCallback _ _ _ _ _ fmt _ =
  error $ "Unsupported sample encoding: " <> show fmt

data Channel= L | R

volC :: Channel -> Layer -> [Sample] -> [Sample]
volC p layer samples =
  let m = case p of
            R -> 1
            L -> -1
      coef = (_layerPan layer * m + 1) * (_layerVol layer)
   in map ( Sample . ( coef * ) . getSample ) samples

aggregateChunks :: [[(Sample, Sample)]] -> [(Sample, Sample)]
aggregateChunks = map ((sum *** sum) . unzip) . transpose

