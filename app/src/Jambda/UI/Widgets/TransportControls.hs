{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.TransportControls
  ( transportControlsWidget
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (writeIORef, modifyIORef')

import           Reflex.Dom

import           Jambda.Types
import           Jambda.Data
import           Jambda.UI.Widgets.ToggleButton (toggleButton)

transportControlsWidget :: JambdaUI t m => JamState -> m ()
transportControlsWidget st = mdo
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

