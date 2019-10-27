{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Jambda.UI.Widgets.Layer.Mute
  ( muteAndSolo
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef (modifyIORef')

import           Reflex
import           Reflex.Dom

import           Jambda.Types

muteAndSolo :: JambdaUI t m
            => JamState
            -> Int
            -> LayerUI
            -> m (Event t LayerEvent)
muteAndSolo st layerId layerUI =
  divClass "mute-control-wrapper" $ do
    -- Mute button
    let muteClass = if layerUI^.layerUIMuted
                       then "mute-btn active"
                       else "mute-btn"
    muteBtn <- divClass muteClass $ button "M"
    let mutedEv = ChangeLayer layerId ( layerUIMuted %~ not ) <$ muteBtn
    performEvent_ $
      muteLayer st layerId ( not $ layerUI^.layerUIMuted ) <$ muteBtn

    -- Solo button
    let soloClass = if layerUI^.layerUISoloed
                       then "solo-btn active"
                       else "solo-btn"
    soloBtn <- divClass soloClass $ button "S"
    let soloedEv = ChangeLayer layerId ( layerUISoloed %~ not ) <$ soloBtn
    performEvent_ $
      soloLayer st layerId ( not $ layerUI^.layerUISoloed ) <$ soloBtn

    pure $ leftmost [mutedEv, soloedEv]

muteLayer :: MonadIO m => JamState -> Int -> Bool -> m ()
muteLayer st idx muted = liftIO $
  modifyIORef' ( st^.jamStLayersRef )
               ( ix idx . layerMuted .~ muted )

soloLayer :: MonadIO m => JamState -> Int -> Bool -> m ()
soloLayer st idx soloed = liftIO $
  modifyIORef' ( st^.jamStLayersRef )
               ( ix idx . layerSoloed .~ soloed )
