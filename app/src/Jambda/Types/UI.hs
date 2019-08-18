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

import           Jambda.Types.Pitch (Pitch)

type JambdaUI t m = (MonadHold t m, MonadFix m, DomBuilder t m, PerformEvent t m, MonadIO (Performable m))

data LayerEvent
  = NewLayer Int LayerUI
  | RemoveLayer Int
  | ChangeLayer Int LayerUI

data LayerUI =
  LayerUI
    { _layerUIBeatCode :: InputState T.Text
    , _layerUIOffset :: InputState T.Text
    , _layerUIPitch :: InputState Pitch
    }

mkNewLayerUI :: T.Text -> T.Text -> Pitch -> LayerUI
mkNewLayerUI beat offset pitch =
  LayerUI
    { _layerUIBeatCode = InputState beat Nothing
    , _layerUIOffset   = InputState offset Nothing
    , _layerUIPitch    = InputState pitch Nothing
    }

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
