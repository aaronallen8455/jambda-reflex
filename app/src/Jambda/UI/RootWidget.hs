{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Jambda.UI.RootWidget
  ( rootWidget
  ) where

import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.IntMap as M
import           Data.IORef (writeIORef, modifyIORef', readIORef)
import           Reflex
import           Reflex.Dom
import           System.Random (randomIO)

import           Jambda.Data
import           Jambda.Types
import           Jambda.UI.Widgets

rootWidget :: (JambdaUI t m, PostBuild t m) => JamState -> m ()
rootWidget st = el "div" $ do
  rec
    newLayerIdDyn <- holdDyn 1 $ maybe 1 (succ . fst) . M.lookupMax
                             <$> current layerMapDyn <@ newLayerEv

    randomNoteEv <- performEvent $ generateRandomNote <$ newLayerEv

    newLayerNoteDyn <- holdDyn ( Pitch ANat 4 ) randomNoteEv
    let newLayerEventDyn = NewLayer
                       <$> newLayerIdDyn
                       <*> ( mkNewLayerUI "1" "0" <$> newLayerNoteDyn )

        layerEvents = leftmost [ updated newLayerEventDyn, editLayerEvents ]
        initLayerMap = M.singleton 1 ( mkNewLayerUI "1" "0" ( Pitch ANat 4 ) )

    layerMapDyn <- foldDyn applyLayerEvent initLayerMap layerEvents

    let layerWidgetsDyn =
          ( \m -> M.traverseWithKey ( \i layerUI -> layerWidget st i layerUI m ) m )
            <$> layerMapDyn

    -- create the layer widgets
    editLayerEvents <- switchHold never . fmap (leftmost . M.elems)
                   =<< dyn layerWidgetsDyn

    newLayerEv <- button "New Layer"

    -- Adds a new layer to the backend
    performEvent_ $ createNewLayer st <$> current newLayerIdDyn
                                      <@> updated newLayerNoteDyn

    playbackStateDyn <- accum (const id)
                              Stopped
                              (leftmost [startEv, stopEv, pauseEv])

    let canPlayDyn  = fmap (/= Playing) playbackStateDyn
        canStopDyn  = fmap (/= Stopped) playbackStateDyn
        canPauseDyn = fmap (== Playing) playbackStateDyn

    startEv <- (Playing <$) <$> toggleButton canPlayDyn "Start"
    performEvent_ $ (liftIO $ st^.jamStStartPlayback) <$ startEv

    let stopAction = liftIO $ do
          st^.jamStStopPlayback
          writeIORef ( st^.jamStElapsedSamples ) 0
          modifyIORef' ( st^.jamStLayersRef ) ( fmap resetLayer )

    stopEv <- (Stopped <$) <$> toggleButton canStopDyn "Stop"
    performEvent_ $ stopAction <$ stopEv

    pauseEv <- (Paused <$) <$> toggleButton canPauseDyn "Pause"
    performEvent_ $ (liftIO $ st^.jamStStopPlayback) <$ pauseEv

  -- tempo
  text "Tempo: "
  tempoDyn <- numberInput (120.0 :: BPM) parseBpm bpmToText 1
  performEvent_ $ changeTempo st <$> updated tempoDyn

  -- volume
  text "Vol.: "
  volumeDyn <- numberInput (5.0 :: Vol) parseVol volToText 0.2
  performEvent_ $ changeVol st <$> updated volumeDyn

  pure ()

applyLayerEvent :: LayerEvent -> M.IntMap LayerUI -> M.IntMap LayerUI
applyLayerEvent (NewLayer i l) = M.insert i l
applyLayerEvent (RemoveLayer i) = M.delete i
applyLayerEvent (ChangeLayer i l) = at i . _Just .~ l

createNewLayer :: MonadIO m => JamState -> Int -> Pitch -> m ()
createNewLayer st idx pitch = liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
  elapsedSamples <- readIORef ( st^.jamStElapsedSamples )
  tempo <- readIORef ( st^.jamStTempoRef )
  let elapsedCells =
        numSamplesToCellValue tempo elapsedSamples
      layer = newLayer pitch

  void $ modifyIORef' ( st^.jamStLayersRef )
                      ( fmap ( syncLayer elapsedCells )
                      . ( at idx ?~ layer )
                      )

changeTempo :: MonadIO m => JamState -> BPM -> m ()
changeTempo st newTempo = liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
  currentTempo <- readIORef $ st^.jamStTempoRef
  let ratio = getBPM $ currentTempo / newTempo

  modifyIORef' ( st^.jamStElapsedSamples ) ( * ratio )
  writeIORef ( st^.jamStTempoRef ) newTempo

changeVol :: MonadIO m => JamState -> Vol -> m ()
changeVol st newVol = liftIO $ do
  writeIORef ( st^.jamStVolumeRef ) newVol

generateRandomNote :: MonadIO m => m Pitch
generateRandomNote = liftIO randomIO
