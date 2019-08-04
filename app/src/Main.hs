{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Lens
import           Control.Monad (guard)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.IntMap as M
import           Reflex
import           Reflex.Dom
import           Text.Read (readMaybe)

import           Jambda.Data
import           Jambda.Types

main :: IO ()
main = do
  -- set up the backend stuff
  -- - MVar with the layers models
  -- - MVar with the tempo
  -- - MVar with the volume
  -- will supply them to the network via ReaderT

  css <- BS.readFile "../css/styles.css"
  mainWidgetWithCss css $ el "div" $ do
    rec
      newLayerIdDyn <- count newLayerEv
      randomNoteEv <- performEvent $ generateRandomNote <$ newLayerEv
      -- TODO need to perform layer adding and removing IO actions
      newLayerNoteDyn <- accum (const id) (Note "A4") randomNoteEv
      let newLayerEventDyn = NewLayer <$> newLayerIdDyn <*> newLayerNoteDyn
          layerEvents = leftmost [updated newLayerEventDyn, deleteLayerEvents]

      layerMapDyn <- foldDyn applyLayerEvent mempty layerEvents
      let layerWidgetsDyn = (\m -> M.traverseWithKey (\i note -> layerWidget i note m))
                        <$> layerMapDyn

      -- create the layer widgets
      deleteLayerEvents <- switchHold never . fmap (leftmost . M.elems)
                       =<< dyn layerWidgetsDyn

      newLayerEv <- button "New Layer"

      playbackStateDyn <- accum (const id)
                                Stopped
                                (leftmost [startEv, stopEv, pauseEv])

      let canPlayDyn  = fmap (/= Playing) playbackStateDyn
          canStopDyn  = fmap (/= Stopped) playbackStateDyn
          canPauseDyn = fmap (== Playing) playbackStateDyn

      startEv <- (Playing <$) <$> toggleButton canPlayDyn "Start"
      performEvent_ $ (liftIO $ putStrLn "Starting") <$ startEv

      stopEv <- (Stopped <$) <$> toggleButton canStopDyn "Stop"
      performEvent_ $ (liftIO $ putStrLn "Stopping") <$ stopEv

      pauseEv <- (Paused <$) <$> toggleButton canPauseDyn "Pause"
      performEvent_ $ (liftIO $ putStrLn "Pausing") <$ pauseEv

    -- tempo
    text "Tempo: "
    tempoDyn <- numberInput 120.0 (> 0)
    performEvent_ $ (liftIO $ putStrLn "Changing tempo") <$ updated tempoDyn
    -- volume
    text "Vol.: "
    volumeDyn <- numberInput 5.0 (\x -> x >= 0 && x <= 10)
    performEvent_ $ (liftIO $ putStrLn "Changing volume") <$ updated volumeDyn

    pure ()

layerWidget :: (MonadHold t m, MonadFix m, DomBuilder t m, PerformEvent t m, MonadIO (Performable m))
            => Int -> Note -> M.IntMap (Note, T.Text) -> m (Event t LayerEvent)
layerWidget layerId note layerInfo = el "div" $ do
  el "div" . text $ "Layer " <> showText layerId

  let valdiateBeatCode t = do
        code <- parseBeat
  beatCodeInput <- textFieldInput "1" (BeatCode "1") (Just . BeatCode) (const never)

  let beatCodeEv = fmap (updateLayerBeatCode layerId)
                 $ updated beatCodeInput
  performEvent_ beatCodeEv

  noteInput <- textFieldInput "A4" (Note "A4") (Just . Note) (const never)

  let noteEv = fmap (updateLayerNote layerId)
             $ updated noteInput
  performEvent_ noteEv

  deleteEv <- button "X"

  pure $ RemoveLayer layerId <$ deleteEv

-- | an input field that only updates its value on blur or pressing enter, and
-- only if the text contents are successfully validated.
textFieldInput :: (MonadHold t m, MonadFix m, DomBuilder t m, Eq a)
               => T.Text
               -> a
               -> (T.Text -> Maybe a)
               -> (Event t Word -> Event t T.Text)
               -> m (Dynamic t a)
textFieldInput initialText initialValue validator mkSetValEv = do
  rec
    input <- inputElement $
      def & inputElementConfig_initialValue .~ initialText
          & inputElementConfig_setValue .~ setValEv
          & inputElementConfig_elementConfig
              %~ elementConfig_modifyAttributes .~ attrEv

    let element    = _inputElement_element input
        keydownEv  = domEvent Keydown element
        keypressEv = domEvent Keypress element
        setValEv   = mkSetValEv keydownEv
        editingEv  = ffilter (not . flip elem [37, 38, 39, 40]) keydownEv
        blurEv     = domEvent Blur element
        enterKeyEv = () <$ ffilter (== 13) keypressEv
        valueEv    = leftmost
                       [ setValEv
                       , tagPromptlyDyn
                           (_inputElement_value input)
                           $ leftmost [ blurEv
                                      , enterKeyEv
                                      ]
                       ]

    mbValDyn <- foldDyn (const . validator) (Just initialValue) valueEv

    let invalidAttrs  = "class" =: Just "invalid-field"
        editingAttrs  = "class" =: Just "edited-field"
        validAttrs    = "class" =: Nothing
        invalidAttrEv = ffor (updated mbValDyn) $ maybe invalidAttrs
                                                        (const validAttrs)
        editingAttrEv = editingAttrs <$ editingEv
        attrEv        = leftmost [invalidAttrEv, editingAttrEv]
    valDyn <- foldDynMaybe const initialValue $ updated mbValDyn
  holdUniqDyn valDyn

numberInput :: (MonadHold t m, MonadFix m, DomBuilder t m, Show a, Read a, Num a, Eq a)
            => a -> (a -> Bool) -> m (Dynamic t a)
numberInput startingValue validator = do
  rec
    let validate v = do
          n <- readMaybe $ T.unpack v
          guard $ validator n
          pure n
        s ev =
          let upArrowEv   = 1 <$ ffilter (== 38) ev
              downArrowEv = (-1) <$ ffilter (== 40) ev
              setVal cur delta
                | validator new = Just $ showText new
                | otherwise = Nothing
                where new = cur + delta
              setValEv = fmapMaybe id $ setVal <$> current input
                                               <@> leftmost [upArrowEv, downArrowEv]
           in setValEv

    input <- textFieldInput (showText startingValue) startingValue validate s

  pure input

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

-- the list function takes a Map Int Layer and a function which turns a
-- Dynamic Layer into a widget. Is it necessary for the Layer to be in a
-- Dynamic since all the effects that will occur on the layer will happen
-- form within the widget itself?

applyLayerEvent :: LayerEvent -> M.IntMap (Note, T.Text) -> M.IntMap (Note, T.Text)
applyLayerEvent (NewLayer i note) = M.insert i (note, "1")
applyLayerEvent (RemoveLayer i) = M.delete i
applyLayerEvent (ChangeBeatCode i code) = at i . _Just . _2 .~ code

updateLayerBeatCode :: MonadIO m => Int -> BeatCode -> m ()
updateLayerBeatCode layerId newBeatCode = liftIO $
  putStrLn $ "Update beatcode of Layer " <> show layerId <> " to " <> show newBeatCode

updateLayerNote :: MonadIO m => Int -> Note -> m ()
updateLayerNote layerId newNote = liftIO $
  putStrLn $ "Update note of Layer " <> show layerId <> " to " <> show newNote

generateRandomNote :: MonadIO m => m Note
generateRandomNote = liftIO $ do
  putStrLn "Generating a random note"
  pure (Note "A4")

showText :: Show a => a -> T.Text
showText = T.pack . show

data LayerEvent
  = NewLayer Int Note
  | RemoveLayer Int
  | ChangeBeatCode Int T.Text

data Layer = Layer

newtype Note = Note T.Text deriving (Show, Eq)

data PlaybackState
  = Stopped
  | Playing
  | Paused
  deriving (Show, Eq)

newtype BeatCode = BeatCode T.Text deriving (Show, Eq)
