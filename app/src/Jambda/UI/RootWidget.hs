{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.RootWidget
  ( rootWidget
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Reflex
import           Reflex.Dom

import           Jambda.Types
import           Jambda.UI.Widgets

rootWidget :: JambdaUI t m => JamState -> [BeatFileName] -> m ()
rootWidget st savedBeats = el "div" $ mdo
  -- quit button
  quitEvDyn <- widgetHold ( button "Quit" ) (pure never <$ quitEv)
  quitEv <- sample $ current quitEvDyn

  performEvent_ $ liftIO ( st^.jamStFinalizer ) <$ quitEv

  _ <- flip widgetHold ( text "Have a nice day!" <$ quitEv ) $ mdo
    layerMapDyn <- layerListWidget st newLayerEvent' loadedLayersEv'

    ( newLayerEvent', loadedLayersEv' ) <- divClass "control-wrapper" $ mdo
      newLayerEvent <- newLayerWidget st layerMapDyn

      transportControlsWidget st

      tempoWidget st loadedTempoEv

      volumeWidget st

      ( loadedLayersEv, loadedTempoEv )
        <- persistenceWidget st savedBeats layerMapDyn

      pure ( newLayerEvent, loadedLayersEv )

    pure ()

  pure ()
