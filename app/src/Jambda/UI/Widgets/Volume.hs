{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.Volume
  ( volumeWidget
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef (writeIORef)

import           Reflex.Dom

import           Jambda.Types
import           Jambda.Data
import           Jambda.UI.Widgets.NumberInput (numberInput)

volumeWidget :: JambdaUI t m => JamState -> m ()
volumeWidget st = do
  volumeDyn <- numberInput (Just "Volume")
                           (Just "volume-input")
                           (5.0 :: Vol)
                           parseVol
                           volToText
                           0.2
  performEvent_ $ changeVol st <$> updated volumeDyn

changeVol :: MonadIO m => JamState -> Vol -> m ()
changeVol st newVol = liftIO $ do
  writeIORef ( st^.jamStVolumeRef ) newVol
