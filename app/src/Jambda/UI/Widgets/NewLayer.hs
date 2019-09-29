{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.NewLayer
  ( newLayerWidget
  ) where

import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.IntSet as S
import           Data.IORef (modifyIORef', readIORef)
import           System.Random (randomIO)

import           Reflex.Dom

import           Jambda.Data
import           Jambda.Types

newLayerWidget :: JambdaUI t m => JamState -> Dynamic t S.IntSet -> m (Event t LayerEvent)
newLayerWidget st layerIdxDyn = mdo
  newLayerIdB <- hold 1 $ maybe 1 (succ . fst) . S.maxView
                      <$> current layerIdxDyn <@ newLayerEv

  randomNoteEv <- fmap SSPitch <$> ( performEvent $ generateRandomNote <$ newLayerEv )

  newLayerEv <- button "New Layer"

  -- Adds a new layer to the backend
  performEvent_ $ createNewLayer st <$> newLayerIdB
                                    <@> randomNoteEv

  pure $ NewLayer
          <$> newLayerIdB
          <@> ( mkNewLayerUI "1" "0" <$> randomNoteEv )

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

generateRandomNote :: MonadIO m => m Pitch
generateRandomNote = liftIO randomIO
