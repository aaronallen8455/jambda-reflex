{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Jambda.UI.Widgets.Layer.Source
  ( mkPitchInput
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)

import           Reflex
import           Reflex.Dom

import           Jambda.Data (applyLayerSourceChange, parsePitch)
import           Jambda.Types
import           Jambda.UI.Widgets.TextField (textFieldInput)

mkPitchInput :: JambdaUI t m
             => JamState -> Int -> LayerUI -> m (Event t LayerEvent)
mkPitchInput st layerId layerUI = do
  pitchInput <- textFieldInput (pitchText <$> _layerUIPitch layerUI)
                               parsePitch
                               (const never)

  let pitchEv = fforMaybe pitchInput $
        either (const Nothing)
               (Just . liftIO . applyLayerSourceChange st layerId)

  performEvent_ pitchEv
  pure . ffor pitchInput $ \case
    Left inv -> ChangeLayer layerId (layerUI & layerUIPitch . inpInvalid .~ Just inv)
    Right n  -> ChangeLayer layerId (layerUI & layerUIPitch . inpValid .~ n
                                             & layerUIPitch . inpInvalid .~ Nothing
                                    )

