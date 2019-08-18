module Jambda.UI.Widgets.ToggleButton
  ( toggleButton
  ) where

import qualified Data.Text as T

import           Reflex
import           Reflex.Dom

toggleButton :: (DomBuilder t m, PostBuild t m)
             => Dynamic t Bool
             -> T.Text
             -> m (Event t ())
toggleButton enabledDyn label = do
  let attrs = ffor enabledDyn $ \e ->
        if e
           then mempty
           else "disabled" =: "disabled"
  (btn, _) <- elDynAttr' "button" attrs $ text label
  pure $ () <$ domEvent Click btn

