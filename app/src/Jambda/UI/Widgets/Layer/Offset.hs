{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.Layer.Offset
  ( mkOffsetInput
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)

import           Reflex

import           Jambda.Data (applyLayerOffsetChange, parseOffset)
import           Jambda.Types
import           Jambda.UI.Widgets.TextField

mkOffsetInput :: JambdaUI t m
              => JamState -> Int -> LayerUI -> m (Event t LayerEvent)
mkOffsetInput st layerId layerUI = do
  let validateOffset t = do
        offset <- parseOffset t
        pure (offset, t)

  offsetInput <- textFieldInput "Offset"
                                (_layerUIOffset layerUI)
                                validateOffset
                                (const never)

  let offsetEv = fforMaybe offsetInput $
        either (const Nothing)
               (Just . liftIO . applyLayerOffsetChange st layerId . fst)

  performEvent_ offsetEv

  pure . ffor offsetInput $ \case
    Left inv     -> ChangeLayer
                      layerId
                      (layerUI & layerUIOffset . inpInvalid .~ Just inv)
    Right (_, t) -> ChangeLayer
                      layerId
                      (layerUI & layerUIOffset . inpInvalid .~ Nothing
                               & layerUIOffset . inpValid .~ t
                      )

