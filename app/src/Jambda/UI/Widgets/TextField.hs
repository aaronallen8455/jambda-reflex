{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.TextField
  ( textFieldInput
  ) where

import           Control.Lens
import qualified Data.Text as T

import           Reflex
import           Reflex.Dom

import           Jambda.Types
import           Jambda.UI.Widgets.Label (label)

-- | an input field that only updates its value on blur or pressing enter, and
-- only if the text contents are successfully validated.
textFieldInput :: JambdaUI t m
               => Maybe T.Text
               -> Maybe T.Text
               -> InputState T.Text
               -> (T.Text -> Maybe a)
               -> (Event t Word -> Event t T.Text)
               -> m (Event t (Either T.Text a))
textFieldInput mbLabel class' inpState validator mkSetValEv = do
  let invalidAttrs  = "class" =: (Just ("invalid-field") <> fmap (" " <>) class')
      editingAttrs  = "class" =: (Just ("edited-field") <> fmap (" " <>) class')
      validAttrs    = "class" =: class'
      (curText, curAttr) =
        case inpState of
          InputState _ (Just t) -> (t, fmap (maybe mempty id) invalidAttrs)
          InputState t _        -> (t, fmap (maybe mempty id) validAttrs)

  rec
    input <- maybe id label mbLabel . inputElement $
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
        updateUIEv = ffilter filterNoOp
                   . tagPromptlyDyn
                         (_inputElement_value input)
                         $ leftmost [ blurEv
                                    , enterKeyEv
                                    ]
        valueEv    = validate <$> leftmost
                       [ setValEv
                       , updateUIEv
                       ]

    let filterNoOp v
          | Just v == _inpInvalid inpState = False
          | _inpInvalid inpState == Nothing && v == _inpValid inpState = False
          | otherwise = True
        validate v = maybe (Left v) Right $ validator v
        invalidAttrEv = ffor valueEv $ either (const invalidAttrs)
                                              (const validAttrs)
        editingAttrEv = editingAttrs <$ editingEv
        attrEv        = leftmost [invalidAttrEv, editingAttrEv]

  pure valueEv
