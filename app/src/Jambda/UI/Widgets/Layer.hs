{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.Layer
  ( layerWidget
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.IntMap as M
import           Data.IORef (modifyIORef')
import qualified Data.Text as T

import           Reflex
import           Reflex.Dom

import           Jambda.Types
import           Jambda.Data
import           Jambda.UI.Widgets.Layer.BeatCode
import           Jambda.UI.Widgets.Layer.Source
import           Jambda.UI.Widgets.Layer.Offset

layerWidget :: JambdaUI t m
            => JamState -> Int -> LayerUI -> M.IntMap LayerUI -> m (Event t LayerEvent)
layerWidget st layerId layerUI layerMap = el "div" $ do
  el "div" . text $ "Layer " <> (T.pack . show) layerId

  -- Beatcode input
  changeBeatCodeEv <- mkBeatCodeInput st layerMap layerId layerUI

  -- Pitch Input
  changeSourceEv <- mkSourceInput st layerId layerUI

  -- Offset input
  changeOffsetEv <- mkOffsetInput st layerId layerUI

  -- Vol. input
  volInput <- rangeInput $ def & rangeInputConfig_initialValue .~ _layerUIVol layerUI
                               & attributes .~ constDyn ("max" =: "1" <> "step" =: ".05")

  let volEv = current (_rangeInput_value volInput) <@ _rangeInput_mouseup volInput
      changeVolEv = ffor volEv $ \v -> ChangeLayer layerId ( layerUI & layerUIVol .~ v )

  performEvent_ $ liftIO . applyLayerVolChange st layerId <$> volEv

  -- Pan input
  panInput <- rangeInput $ def & rangeInputConfig_initialValue .~ _layerUIPan layerUI
                               & attributes .~ constDyn ("max" =: "1" <> "min" =: "-1" <> "step" =: ".1")
  let panEv = current (_rangeInput_value panInput) <@ _rangeInput_mouseup panInput
      changePanEv = ffor panEv $ \p -> ChangeLayer layerId ( layerUI & layerUIPan .~ p )

  performEvent_ $ liftIO . applyLayerPanChange st layerId <$> panEv

  -- Delete button
  deleteEv <- ( RemoveLayer layerId <$ ) <$> button "X"
  performEvent_ $ deleteLayer st layerId <$ deleteEv

  pure $ leftmost [ deleteEv
                  , changeBeatCodeEv
                  , changeSourceEv
                  , changeOffsetEv
                  , changeVolEv
                  , changePanEv
                  ]

deleteLayer :: MonadIO m => JamState -> Int -> m ()
deleteLayer st idx = liftIO $
  modifyIORef' ( st^.jamStLayersRef )
               ( sans idx )
