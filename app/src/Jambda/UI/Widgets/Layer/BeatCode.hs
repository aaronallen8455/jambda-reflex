{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.Layer.BeatCode
  ( mkBeatCodeInput
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.IntMap as M
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import qualified Data.Vector as V

import           Reflex

import           Jambda.Data (applyLayerBeatChange, parseBeat)
import           Jambda.Types
import           Jambda.UI.Widgets.TextField (textFieldInput)

validateBeatCode :: M.IntMap LayerUI -> V.Vector Wav -> Int -> T.Text -> Maybe (M.IntMap (NonEmpty Cell'), T.Text)
validateBeatCode layerMap wavs layerId t = do
  let beatCodes' = _inpValid . _layerUIBeatCode <$> layerMap
      beatCodes'' = M.insert layerId t beatCodes'
      parse i = parseBeat i beatCodes' wavs
  parsedCodes <- M.traverseWithKey parse beatCodes''
  pure (parsedCodes, t)

mkBeatCodeInput :: JambdaUI t m
                => JamState -> M.IntMap LayerUI -> Int -> LayerUI -> m (Event t LayerEvent)
mkBeatCodeInput st layerMap layerId layerUI = do
  beatCodeInput <- textFieldInput (Just "BeatCode")
                                  (Just "beatcode-input")
                                  (_layerUIBeatCode layerUI)
                                  (validateBeatCode layerMap (_jamStWavSources st) layerId)
                                  (const never)

  let beatCodeEv = fforMaybe beatCodeInput $ \case
        Left _ -> Nothing
        Right (v, _) -> Just . liftIO $ applyLayerBeatChange st v

  performEvent_ beatCodeEv

  pure . ffor beatCodeInput $ \case
    Left inv        -> ChangeLayer layerId ( layerUIBeatCode . inpInvalid .~ Just inv )
    Right (_, txt)  -> ChangeLayer layerId $ ( layerUIBeatCode . inpValid .~ txt )
                                           . ( layerUIBeatCode . inpInvalid .~ Nothing )
