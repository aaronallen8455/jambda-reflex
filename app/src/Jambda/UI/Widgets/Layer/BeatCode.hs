{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Jambda.UI.Widgets.Layer.BeatCode
  ( mkBeatCodeInput
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.IntMap as M
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T

import           Reflex
import           Reflex.Dom

import           Jambda.Data (applyLayerBeatChange, parseBeat)
import           Jambda.Types
import           Jambda.UI.Widgets.TextField (textFieldInput)

validateBeatCode :: M.IntMap LayerUI -> Int -> T.Text -> Maybe (M.IntMap (NonEmpty Cell'), T.Text)
validateBeatCode layerMap layerId t = do
  let beatCodes' = _inpValid . _layerUIBeatCode <$> layerMap
      beatCodes'' = M.insert layerId t beatCodes'
      parse i = parseBeat i beatCodes'
  parsedCodes <- M.traverseWithKey parse beatCodes''
  pure (parsedCodes, t)

mkBeatCodeInput :: JambdaUI t m
                => JamState -> M.IntMap LayerUI -> Int -> LayerUI -> m (Event t LayerEvent)
mkBeatCodeInput st layerMap layerId layerUI = do
  beatCodeInput <- textFieldInput (_layerUIBeatCode layerUI)
                                  (validateBeatCode layerMap layerId)
                                  (const never)

  let beatCodeEv = fforMaybe beatCodeInput $ \case
        Left _ -> Nothing
        Right (v, _) -> Just . liftIO $ applyLayerBeatChange st v

  performEvent_ beatCodeEv

  pure . ffor beatCodeInput $ \case
    Left inv        -> ChangeLayer layerId (layerUI & layerUIBeatCode . inpInvalid .~ Just inv)
    Right (_, txt)  -> ChangeLayer layerId (layerUI & layerUIBeatCode . inpValid .~ txt
                                                    & layerUIBeatCode . inpInvalid .~ Nothing
                                           )
