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

rootWidget :: JambdaUI t m => JamState -> m ()
rootWidget st = el "div" $ mdo
  -- quit button
  quitEvDyn <- widgetHold ( button "Quit" ) (pure never <$ quitEv)
  quitEv <- sample $ current quitEvDyn

  performEvent_ $ liftIO ( st^.jamStFinalizer ) <$ quitEv

  _ <- flip widgetHold ( text "Have a nice day!" <$ quitEv ) $ mdo
    layerIdxs <- layerListWidget st newLayerEvent'

    newLayerEvent' <- divClass "control-wrapper" $ do
      newLayerEvent <- newLayerWidget st layerIdxs

      transportControlsWidget st

      tempoWidget st

      volumeWidget st

      pure newLayerEvent

    pure ()

  pure ()
