{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.Layer
  ( layerWidget
  ) where

import qualified Data.IntMap as M
import qualified Data.Text as T

import           Reflex
import           Reflex.Dom

import           Jambda.Types
import           Jambda.UI.Widgets.Layer.BeatCode
import           Jambda.UI.Widgets.Layer.Delete
import           Jambda.UI.Widgets.Layer.Source
import           Jambda.UI.Widgets.Layer.Mute
import           Jambda.UI.Widgets.Layer.Offset
import           Jambda.UI.Widgets.Layer.VolAndPan

layerWidget :: JambdaUI t m
            => JamState
            -> Int
            -> LayerUI
            -> M.IntMap LayerUI
            -> m (Event t LayerEvent)
layerWidget st layerId layerUI layerMap = divClass "layer-wrapper" $ do
  el "div" . text $ "Layer " <> (T.pack . show) layerId

  -- Mute and solo controls
  muteAndSoloEv    <- muteAndSolo st layerId layerUI

  -- Beatcode input
  changeBeatCodeEv <- mkBeatCodeInput st layerMap layerId layerUI

  -- Pitch Input
  changeSourceEv   <- mkSourceInput st layerId layerUI

  -- Offset input
  changeOffsetEv   <- mkOffsetInput st layerId layerUI

  -- Volume and pan controls
  volAndPanEv      <- volAndPan st layerId layerUI

  -- Delete button
  deleteEv         <- deleteButton st layerId

  pure $ leftmost [ deleteEv
                  , changeBeatCodeEv
                  , changeSourceEv
                  , changeOffsetEv
                  , volAndPanEv
                  , muteAndSoloEv
                  ]
