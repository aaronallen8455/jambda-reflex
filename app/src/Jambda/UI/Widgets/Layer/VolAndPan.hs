{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Jambda.UI.Widgets.Layer.VolAndPan
  ( volAndPan
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Reflex
import           Reflex.Dom

import           Jambda.Data
import           Jambda.Types
import           Jambda.UI.Widgets.Label (label)

volAndPan :: JambdaUI t m
          => JamState
          -> Int
          -> LayerUI
          -> m (Event t LayerEvent)
volAndPan st layerId layerUI = do
  -- Vol. input
  volInput <- label "Volume"
            . rangeInput $ def & rangeInputConfig_initialValue .~ _layerUIVol layerUI
                               & attributes .~ constDyn ("max" =: "1" <> "step" =: ".05")

  let volEv = current (_rangeInput_value volInput) <@ _rangeInput_mouseup volInput
      changeVolEv = ffor volEv $ \v -> ChangeLayer layerId ( layerUIVol .~ v )

  performEvent_ $ liftIO . applyLayerVolChange st layerId <$> _rangeInput_input volInput

  -- Pan input
  panInput <- label "Pan"
            . rangeInput $ def & rangeInputConfig_initialValue .~ _layerUIPan layerUI
                               & attributes .~ constDyn ("max" =: "1" <> "min" =: "-1" <> "step" =: ".1")

  let panEv = current (_rangeInput_value panInput) <@ _rangeInput_mouseup panInput
      changePanEv = ffor panEv $ \p -> ChangeLayer layerId ( layerUIPan .~ p )

  performEvent_ $ liftIO . applyLayerPanChange st layerId <$> _rangeInput_input panInput

  pure $ leftmost [changeVolEv, changePanEv]
