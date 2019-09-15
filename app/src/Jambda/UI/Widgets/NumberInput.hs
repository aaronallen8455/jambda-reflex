{-# LANGUAGE RecursiveDo #-}
module Jambda.UI.Widgets.NumberInput
  ( numberInput
  ) where

import           Control.Monad.Fix (MonadFix)
import qualified Data.Text as T

import           Reflex
import           Reflex.Dom

import           Jambda.Types
import           Jambda.UI.Widgets.TextField (textFieldInput)

numberInput :: (MonadHold t m, MonadFix m, DomBuilder t m, Num r)
            => T.Text
            -> r
            -> (T.Text -> Maybe r)
            -> (r -> T.Text)
            -> r
            -> m (Dynamic t r)
numberInput label initialVal validator toText delta = do
  rec
    let s ev =
          let upArrowEv    = ( + delta ) <$ ffilter ( == ( 38 :: Word ) ) ev
              downArrowEv  = ( subtract delta ) <$ ffilter ( == ( 40 :: Word ) ) ev
              setVal cur f = toText <$> ( validator . toText ) new
                where new  = f cur
           in fmapMaybe id $ setVal <$> current inputDyn
                                    <@> leftmost [upArrowEv, downArrowEv]

    input <- textFieldInput label
                            ( InputState (toText initialVal) Nothing )
                            validator
                            s

    let validInput = fforMaybe input $ either (const Nothing) Just
    inputDyn <- holdDyn initialVal validInput

  pure inputDyn
