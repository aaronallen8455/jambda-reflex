{-# LANGUAGE FlexibleContexts #-}
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

  -- Delete button
  deleteEv <- ( RemoveLayer layerId <$ ) <$> button "X"
  performEvent_ $ deleteLayer st layerId <$ deleteEv

  pure $ leftmost [ deleteEv
                  , changeBeatCodeEv
                  , changeSourceEv
                  , changeOffsetEv
                  ]

deleteLayer :: MonadIO m => JamState -> Int -> m ()
deleteLayer st idx = liftIO $
  modifyIORef' ( st^.jamStLayersRef )
               ( sans idx )
