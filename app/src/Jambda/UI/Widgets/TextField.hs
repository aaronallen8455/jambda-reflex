{-# LANGUAGE RecursiveDo #-}
module Jambda.UI.Widgets.TextField
  ( textFieldInput
  ) where

import           Control.Lens
import           Control.Monad.Fix (MonadFix)
import qualified Data.Text as T

import           Reflex
import           Reflex.Dom

import           Jambda.Types

-- | an input field that only updates its value on blur or pressing enter, and
-- only if the text contents are successfully validated.
textFieldInput :: (MonadHold t m, MonadFix m, DomBuilder t m)
               => T.Text
               -> InputState T.Text
               -> (T.Text -> Maybe a)
               -> (Event t Word -> Event t T.Text)
               -> m (Event t (Either T.Text a))
textFieldInput "" inpState validator mkSetValEv = do
  let invalidAttrs  = "class" =: Just "invalid-field"
      editingAttrs  = "class" =: Just "edited-field"
      validAttrs    = "class" =: Nothing
      (curText, curAttr) =
        case inpState of
          InputState _ (Just t) -> (t, fmap (maybe mempty id) invalidAttrs)
          InputState t _        -> (t, fmap (maybe mempty id) validAttrs)

  rec
    input <- inputElement $
      def & inputElementConfig_initialValue .~ curText
          & inputElementConfig_setValue .~ setValEv
          & inputElementConfig_elementConfig
              %~ ( elementConfig_modifyAttributes  .~ attrEv )
               . ( elementConfig_initialAttributes .~ curAttr )

    let element'   = _inputElement_element input
        keydownEv  = domEvent Keydown element' -- TODO use the 'keypress' fn?
        keypressEv = domEvent Keypress element'
        setValEv   = mkSetValEv keydownEv
        editingEv  = ffilter (not . flip elem [37, 38, 39, 40]) keydownEv
        blurEv     = domEvent Blur element'
        enterKeyEv = () <$ ffilter (== 13) keypressEv
        valueEv    = leftmost
                       [ setValEv
                       , tagPromptlyDyn
                           (_inputElement_value input)
                           $ leftmost [ blurEv
                                      , enterKeyEv
                                      ]
                       ]

    let validate v
          | Just v == _inpInvalid inpState = Nothing
          | _inpInvalid inpState == Nothing && v == _inpValid inpState = Nothing
          | otherwise = Just $ maybe (Left v) Right $ validator v
        eitherValEv   = fmapMaybe id $ validate <$> valueEv
        invalidAttrEv = ffor eitherValEv $ either (const invalidAttrs)
                                                  (const validAttrs)
        editingAttrEv = editingAttrs <$ editingEv
        attrEv        = leftmost [invalidAttrEv, editingAttrEv]

  pure eitherValEv

textFieldInput label inpState validator mkSetValEv =
  elClass "div" "input-wrapper" $ do
    elClass "label" "input-label" $ text label
    textFieldInput "" inpState validator mkSetValEv
