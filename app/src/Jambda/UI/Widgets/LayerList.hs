{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.LayerList
  ( layerListWidget
  ) where

import           Control.Lens
import qualified Data.IntMap as M
import qualified Data.IntSet as S

import           Reflex.Dom

import           Jambda.Types
import           Jambda.UI.Widgets.Layer (layerWidget)

layerListWidget :: JambdaUI t m => JamState -> Event t LayerEvent -> m (Dynamic t S.IntSet)
layerListWidget st newLayerEvent = mdo
  editLayerEvents <- switchHold never . fmap (leftmost . M.elems)
                 =<< dyn layerWidgetsDyn

  let layerEvents = leftmost [ newLayerEvent, editLayerEvents ]
      initLayerMap = M.singleton 1 ( mkNewLayerUI "1" "0" ( SSPitch $ Pitch ANat 4 ) )

  layerMapDyn <- foldDyn applyLayerEvent initLayerMap layerEvents

  let layerWidgetsDyn =
        ( \m -> M.traverseWithKey ( \i layerUI -> layerWidget st i layerUI m ) m )
          <$> layerMapDyn

  pure $ M.keysSet <$> layerMapDyn

applyLayerEvent :: LayerEvent -> M.IntMap LayerUI -> M.IntMap LayerUI
applyLayerEvent (NewLayer i l) = M.insert i l
applyLayerEvent (RemoveLayer i) = M.delete i
applyLayerEvent (ChangeLayer i l) = at i . _Just .~ l
