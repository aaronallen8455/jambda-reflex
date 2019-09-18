{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
rootWidget st = el "div" $ mdo
  -- quit button
  quitEvDyn <- widgetHold ( button "Quit" ) (pure never <$ quitEv)
  quitEv <- sample $ current quitEvDyn

  performEvent_ $ liftIO ( st^.jamStFinalizer ) <$ quitEv

  _ <- flip widgetHold ( text "Have a nice day!" <$ quitEv ) $ do
    rec
      newLayerIdB <- hold 1 $ maybe 1 (succ . fst) . M.lookupMax
                          <$> current layerMapDyn <@ newLayerEv

      randomNoteEv <- fmap SSPitch <$> ( performEvent $ generateRandomNote <$ newLayerEv )

      let newLayerEvent = NewLayer
                         <$> newLayerIdB
                         <@> ( mkNewLayerUI "1" "0" <$> randomNoteEv )

          layerEvents = leftmost [ newLayerEvent, editLayerEvents ]
          initLayerMap = M.singleton 1 ( mkNewLayerUI "1" "0" ( SSPitch $ Pitch ANat 4 ) )

      layerMapDyn <- foldDyn applyLayerEvent initLayerMap layerEvents

      let layerWidgetsDyn =
            ( \m -> M.traverseWithKey ( \i layerUI -> layerWidget st i layerUI m ) m )
              <$> layerMapDyn

      -- create the layer widgets
      editLayerEvents <- switchHold never . fmap (leftmost . M.elems)
                     =<< dyn layerWidgetsDyn

      newLayerEv <- button "New Layer"

      -- Adds a new layer to the backend
      performEvent_ $ createNewLayer st <$> newLayerIdB
                                        <@> randomNoteEv

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
    tempoDyn <- numberInput (Just "Tempo")
                            (Just "tempo-input")
                            (120.0 :: BPM)
                            parseBpm
                            bpmToText
                            1
    performEvent_ $ changeTempo st <$> updated tempoDyn

    -- volume
    text "Vol.: "
    volumeDyn <- numberInput (Just "Volume")
                             (Just "volume-input")
                             (5.0 :: Vol)
                             parseVol
                             volToText
                             0.2
    performEvent_ $ changeVol st <$> updated volumeDyn

  pure ()

applyLayerEvent :: LayerEvent -> M.IntMap LayerUI -> M.IntMap LayerUI
applyLayerEvent (NewLayer i l) = M.insert i l
applyLayerEvent (RemoveLayer i) = M.delete i
applyLayerEvent (ChangeLayer i l) = at i . _Just .~ l

createNewLayer :: MonadIO m => JamState -> Int -> SoundSource -> m ()
createNewLayer st idx soundSource = liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
  elapsedSamples <- readIORef ( st^.jamStElapsedSamples )
  tempo <- readIORef ( st^.jamStTempoRef )
  let elapsedCells =
        numSamplesToCellValue tempo elapsedSamples
      layer = newLayer soundSource

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
