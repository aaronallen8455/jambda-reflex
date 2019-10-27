{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Jambda.UI.Widgets.Layer.Delete
  ( deleteButton
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef (modifyIORef')

import           Reflex.Dom

import           Jambda.Types

deleteButton :: JambdaUI t m
             => JamState
             -> Int
             -> m (Event t LayerEvent)
deleteButton st layerId = do
  deleteEv <- divClass "delete-layer"
            $ ( RemoveLayer layerId <$ ) <$> button ""
  performEvent_ $ deleteLayer st layerId <$ deleteEv

  pure deleteEv

deleteLayer :: MonadIO m => JamState -> Int -> m ()
deleteLayer st idx = liftIO $
  modifyIORef' ( st^.jamStLayersRef )
               ( sans idx )
