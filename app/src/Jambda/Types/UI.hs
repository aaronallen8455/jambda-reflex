{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Jambda.Types.UI where

import qualified Data.Text as T
import           Control.Lens
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)

import           Reflex
import           Reflex.Dom

import           Jambda.Types.SoundSource (SoundSource(..))
import           Jambda.Types.SoundSource.Pitch (Pitch(..), Note(..))

type JambdaUI t m = (MonadHold t m, MonadFix m, DomBuilder t m, PerformEvent t m, PostBuild t m, MonadIO (Performable m))

data LayerEvent
  = NewLayer Int LayerUI
  | RemoveLayer Int
  | ChangeLayer Int LayerUI

data LayerUI =
  LayerUI
    { _layerUIBeatCode    :: InputState T.Text -- ^ Holds state for the beatcode input
    , _layerUIOffset      :: InputState T.Text -- ^ State of offset input
    , _layerUISoundSource :: SoundSource -- ^ The current SoundSource
    , _layerUIPitch       :: InputState Pitch -- ^ Holds the state of the pitch input
    }

mkNewLayerUI :: T.Text -> T.Text -> SoundSource -> LayerUI
mkNewLayerUI beat offset soundSource =
  LayerUI
    { _layerUIBeatCode    = InputState beat Nothing
    , _layerUIOffset      = InputState offset Nothing
    , _layerUISoundSource = soundSource
    , _layerUIPitch       = InputState pitch Nothing
    }
    where
      pitch = case soundSource of
                SSPitch p -> p
                _ -> Pitch ANat 4

data InputState a =
  InputState
    { _inpValid :: a
    , _inpInvalid :: Maybe T.Text
    } deriving (Show, Functor)

data PlaybackState
  = Stopped
  | Playing
  | Paused
  deriving (Show, Eq)

makeLenses ''LayerUI
makeLenses ''InputState
