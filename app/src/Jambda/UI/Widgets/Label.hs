{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.Label
  ( label
  ) where

import qualified Data.Text as T

import           Reflex.Dom

import           Jambda.Types

label :: JambdaUI t m => T.Text -> m a -> m a
label txt w =
  elClass "div" "input-wrapper" $ do
    elClass "label" "input-label" $ text txt
    w
