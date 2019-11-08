{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.Types.UI where

import           Data.Aeson.TH
import qualified Data.IntMap as M
import qualified Data.Text as T
import           Control.Lens
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)

import           Reflex
import           Reflex.Dom

import           Jambda.Types.SoundSource (PersistedSoundSource, SoundSourceBase(..), SoundSource)
import           Jambda.Types.SoundSource.Pitch (Pitch(..), Note(..))

type JambdaUI t m = ( MonadHold t m
                    , MonadFix m
                    , DomBuilder t m
                    , PerformEvent t m
                    , PostBuild t m
                    , MonadIO (Performable m)
                    , DomBuilderSpace m ~ GhcjsDomSpace
                    )

data LayerEvent
  = NewLayer Int LayerUI
  | RemoveLayer Int
  | ChangeLayer Int ( LayerUI -> LayerUI )
  | ReplaceLayers ( M.IntMap LayerUI )

data LayerUIBase soundSource =
  LayerUI
    { _layerUIBeatCode    :: InputState T.Text -- ^ Holds state for the beatcode input
    , _layerUIOffset      :: InputState T.Text -- ^ State of offset input
    , _layerUISoundSource :: soundSource -- ^ The current SoundSource
    , _layerUIPitch       :: InputState Pitch -- ^ Holds the state of the pitch input
    , _layerUIVol         :: Float
    , _layerUIPan         :: Float
    , _layerUIMuted       :: Bool
    , _layerUISoloed      :: Bool
    }

type LayerUI = LayerUIBase SoundSource

type PersistedLayerUI = LayerUIBase PersistedSoundSource

mkNewLayerUI :: T.Text -> T.Text -> SoundSource -> LayerUI
mkNewLayerUI beat offset soundSource =
  LayerUI
    { _layerUIBeatCode    = InputState beat Nothing
    , _layerUIOffset      = InputState offset Nothing
    , _layerUISoundSource = soundSource
    , _layerUIPitch       = InputState pitch Nothing
    , _layerUIVol         = 1.0
    , _layerUIPan         = 0.0
    , _layerUIMuted       = False
    , _layerUISoloed      = False
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

makeLenses ''LayerUIBase
makeLenses ''InputState
$(deriveJSON defaultOptions ''InputState)
$(deriveJSON defaultOptions ''LayerUIBase)
