{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.Layer.Source
  ( mkSourceInput
  ) where

import           Control.Arrow ((&&&))
import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import           Reflex
import           Reflex.Dom

import           Jambda.Data (applyLayerSourceChange, parsePitch)
import           Jambda.Types
import           Jambda.UI.Widgets.TextField (textFieldInput)

mkSourceInput :: JambdaUI t m
              => JamState -> Int -> LayerUI -> m (Event t LayerEvent)
mkSourceInput st layerId layerUI = do
  let sourceMap = pure . M.fromList $ ( SSPitch $ layerUI^.layerUIPitch.inpValid, "Pitch" )
                : map ( SSWav &&& mkWavLabel )
                      ( V.toList  $ _jamStWavSources st )
      mkWavLabel w = ( T.pack . show $ _wavIdx w + 1 ) <> ". " <> _wavLabel w

  sourceSelect <- dropdown ( layerUI^.layerUISoundSource ) sourceMap def
  let sourceSelectDyn = _dropdown_value sourceSelect

  curSource <- sample $ current sourceSelectDyn

  pitchEv <- case curSource of
               (SSWav _) -> pure never
               (SSPitch _) -> mkPitchInput layerUI

  let validPitch = fforMaybe pitchEv $ either (const Nothing) (Just . SSPitch)
      srcEvents =  leftmost [ validPitch, updated sourceSelectDyn ]

  performEvent_ $ ffor srcEvents ( liftIO . applyLayerSourceChange st layerId )

  let pitchChangeEv = ffor pitchEv $ \case
        Left inv -> ChangeLayer layerId ( layerUI & layerUIPitch . inpInvalid ?~ inv )
        Right n  -> ChangeLayer layerId ( layerUI & layerUIPitch . inpValid .~ n
                                                  & layerUIPitch . inpInvalid .~ Nothing
                                                  & layerUISoundSource .~ SSPitch n
                                        )

      sourceChangeEv = ffor ( updated sourceSelectDyn ) $ \x ->
        ChangeLayer layerId ( layerUI & layerUISoundSource .~ x )

  pure $ leftmost [ pitchChangeEv, sourceChangeEv ]

mkPitchInput :: JambdaUI t m
             => LayerUI -> m (Event t (Either T.Text Pitch))
mkPitchInput layerUI = do
  textFieldInput Nothing
                 (Just "pitch-input")
                 (pitchText <$> _layerUIPitch layerUI)
                 parsePitch
                 (const never)

