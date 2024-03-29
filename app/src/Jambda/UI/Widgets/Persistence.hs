{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jambda.UI.Widgets.Persistence
  ( persistenceWidget
  ) where

import           Control.Arrow ((&&&))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import           Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import           Reflex.Dom

import           Jambda.Data
import           Jambda.Types
import           Jambda.UI.Widgets.Label (label)
import           Jambda.UI.Widgets.ToggleButton (toggleButton)

persistenceWidget :: forall t m. JambdaUI t m
                  => JamState
                  -> [BeatFileName]
                  -> Dynamic t (IM.IntMap LayerUI)
                  -> m (Event t (IM.IntMap LayerUI), Event t BPM)
persistenceWidget jamSt savedBeats layerMapDyn = mdo
  saveBeatNameEv <- saveBeatWidget jamSt layerMapDyn

  -- Dyn of all beat file names
  savedBeatsDyn
    <- foldDyn ($) ( S.fromList savedBeats ) $
         leftmost
           [ S.insert <$> saveBeatNameEv
           , S.delete <$> deleteBeatNameEv
           ]

  let savedBeatsMap = mkSavedBeatsMap <$> savedBeatsDyn

  ( selectedBeatDyn, loadedBeatEv, loadedTempoEv )
    <- loadBeatWidget jamSt savedBeatsMap

  deleteBeatNameEv <- deleteBeatWidget selectedBeatDyn

  pure ( loadedBeatEv, loadedTempoEv )

saveBeatWidget :: JambdaUI t m
               => JamState
               -> Dynamic t (IM.IntMap LayerUI)
               -> m (Event t BeatFileName)
saveBeatWidget jamSt layerMapDyn = mdo
  -- Value of beat name input
  fileNameDyn <- fmap BeatFileName . _inputElement_value
                   <$> label "Beat Name" ( inputElement def )

  let canSave = not . T.null . getBeatFileName <$> fileNameDyn
  saveBeatEv <- toggleButton canSave "Save"
  let saveBeatNameEv = tag ( current fileNameDyn ) beatSavedEv

  beatSavedEv <- performEvent . fmap liftIO $
    persistBeat jamSt <$> current fileNameDyn
                      <@> tagPromptlyDyn layerMapDyn saveBeatEv

  pure saveBeatNameEv

loadBeatWidget :: JambdaUI t m
                 => JamState
                 -> Dynamic t (M.Map (Maybe BeatFileName) T.Text)
                 -> m ( Dynamic t (Maybe BeatFileName)
                      , Event t (IM.IntMap LayerUI)
                      , Event t BPM
                      )
loadBeatWidget jamSt savedBeatsMap = mdo
  -- Event for selection of a saved beat to load
  selectBeatDropdown <- label "Load Beat" ( dropdown Nothing savedBeatsMap def )

  let selectedBeatEv = _dropdown_change selectBeatDropdown
      selectedBeatDyn = _dropdown_value selectBeatDropdown
  loadTempoAndBeatEv <- performEvent . fmap liftIO $
    (recoverBeat jamSt <$> fmapMaybe id selectedBeatEv)


  let loadTempoAndBeatEv' = fmapMaybe id loadTempoAndBeatEv
      loadedBeatEv = snd <$> loadTempoAndBeatEv'
      loadedTempoEv = fst <$> loadTempoAndBeatEv'

  pure (selectedBeatDyn, loadedBeatEv, loadedTempoEv)

deleteBeatWidget :: JambdaUI t m
                 => Dynamic t (Maybe BeatFileName)
                 -> m (Event t BeatFileName)
deleteBeatWidget selectedBeatDyn = do
  let canDelete = isJust <$> selectedBeatDyn

  deleteBeatEv <- toggleButton canDelete "Delete"

  let deleteBeatNameEv = fmapMaybe id
                       $ tag ( current selectedBeatDyn ) deleteBeatEv

  performEvent_ $ liftIO . deleteSavedBeat <$> deleteBeatNameEv

  pure deleteBeatNameEv

mkSavedBeatsMap :: S.Set BeatFileName -> M.Map (Maybe BeatFileName) T.Text
mkSavedBeatsMap savedBeats | S.null savedBeats = M.singleton Nothing "No saved beats"
mkSavedBeatsMap savedBeats =
    M.fromList $ (Nothing, "Select one") : pairs
  where
    pairs = map (Just &&& getBeatFileName) $ S.toList savedBeats
