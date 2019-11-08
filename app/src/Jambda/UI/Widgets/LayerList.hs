{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.LayerList
  ( layerListWidget
  ) where

import           Control.Lens
import qualified Data.IntMap as M

import           Reflex.Dom

import           Jambda.Types
import           Jambda.UI.Widgets.Layer (layerWidget)

layerListWidget :: JambdaUI t m
                => JamState
                -> Event t LayerEvent
                -> Event t (M.IntMap LayerUI)
                -> m (Dynamic t (M.IntMap LayerUI))
layerListWidget st newLayerEvent loadedLayersEvent = mdo
  editLayerEvents <- switchHold never . fmap (leftmost . M.elems)
                 =<< dyn layerWidgetsDyn

  let layerEvents = leftmost [ newLayerEvent, editLayerEvents, ReplaceLayers <$> loadedLayersEvent ]
      initLayerMap = M.singleton 1 ( mkNewLayerUI "1" "0" ( SSPitch $ Pitch ANat 4 ) )

  layerMapDyn <- foldDyn applyLayerEvent initLayerMap layerEvents

  let layerWidgetsDyn =
        ( \m -> M.traverseWithKey ( \i layerUI -> layerWidget st i layerUI m ) m )
          <$> layerMapDyn

  pure layerMapDyn

applyLayerEvent :: LayerEvent -> M.IntMap LayerUI -> M.IntMap LayerUI
applyLayerEvent (NewLayer i l) = M.insert i l
applyLayerEvent (RemoveLayer i) = M.delete i
applyLayerEvent (ChangeLayer i fn) = ix i %~ fn
applyLayerEvent (ReplaceLayers m) = const m
