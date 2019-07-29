{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Lens
import           Control.Monad (guard)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.Map as M
import           Reflex
import           Reflex.Dom
import           Text.Read (readMaybe)

main :: IO ()
main = do
  -- set up the backend stuff
  -- - MVar with the layers models
  -- - MVar with the tempo
  -- - MVar with the volume
  -- will supply them to the network via ReaderT

  mainWidget $ el "div" $ do
    rec
      newLayerIdDyn <- count newLayerEv
      randomNoteEv <- performEvent $ generateRandomNote <$ newLayerEv
      -- TODO need to perform layer adding and removing IO actions
      newLayerNoteDyn <- accum (const id) Note randomNoteEv
      let newLayerEventDyn = NewLayer <$> newLayerIdDyn <*> newLayerNoteDyn
          layerEvents = leftmost [updated newLayerEventDyn, deleteLayerEvents]

      layerMapDyn <- foldDyn applyLayerEvent mempty layerEvents
      let layerWidgetsDyn = M.traverseWithKey (\i note -> layerWidget i note)
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

layerWidget :: (DomBuilder t m, PerformEvent t m, MonadIO (Performable m))
            => Int -> Note -> m (Event t LayerEvent)
layerWidget layerId note = el "div" $ do
  el "div" . text $ "Layer " <> showText layerId

  beatCodeInput <- inputElement $
    def & inputElementConfig_initialValue .~ "1"

  let beatCodeEv = fmap (updateLayerBeatCode layerId)
                 . updated $ _inputElement_value beatCodeInput
  performEvent_ beatCodeEv

  noteInput <- inputElement $
    def & inputElementConfig_initialValue .~ showText note

  let noteEv = fmap (updateLayerNote layerId)
             . updated $ _inputElement_value noteInput
  performEvent_ noteEv

  deleteEv <- button "X"

  pure $ RemoveLayer layerId <$ deleteEv

numberInput :: (MonadHold t m, MonadFix m, DomBuilder t m, Show a, Read a, Num a, Eq a)
            => a -> (a -> Bool) -> m (Dynamic t a)
numberInput startingValue validator = do
  rec
    input <- inputElement $
      def & inputElementConfig_initialValue .~ showText startingValue
          & inputElementConfig_setValue .~ setValEv

    let element = _inputElement_element input
        validate v _ = do
          n <- readMaybe $ T.unpack v
          guard $ validator n
          pure n

        blurEvent     = domEvent Blur element
        keyDownEv     = domEvent Keydown element
        upArrowEv     = 1 <$ ffilter (== 38) keyDownEv
        downArrowEv   = (-1) <$ ffilter (== 40) keyDownEv
        enterKeyEvent = () <$ ffilter (== 13) keyDownEv

        setVal cur delta
          | validator new = Just $ showText new
          | otherwise = Nothing
          where new = cur + delta
        setValEv = fmapMaybe id $ setVal <$> current valDyn
                                         <@> leftmost [upArrowEv, downArrowEv]
        changeValueEv =
          leftmost [ tagPromptlyDyn (_inputElement_value input)
                                    (leftmost [blurEvent, enterKeyEvent])
                   , setValEv
                   ]

    valDyn <- foldDynMaybe validate startingValue changeValueEv

    -- TODO indicate if text is invalid
  holdUniqDyn valDyn

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

applyLayerEvent :: LayerEvent -> M.Map Int Note -> M.Map Int Note
applyLayerEvent (NewLayer i note) = M.insert i note
applyLayerEvent (RemoveLayer i) = M.delete i

updateLayerBeatCode :: MonadIO m => Int -> T.Text -> m ()
updateLayerBeatCode layerId newBeatCode = liftIO $
  putStrLn $ "Update beatcode of Layer " <> show layerId <> " to " <> T.unpack newBeatCode

updateLayerNote :: MonadIO m => Int -> T.Text -> m ()
updateLayerNote layerId newNote = liftIO $
  putStrLn $ "Update note of Layer " <> show layerId <> " to " <> T.unpack newNote

generateRandomNote :: MonadIO m => m Note
generateRandomNote = liftIO $ do
  putStrLn "Generating a random note"
  pure Note

showText :: Show a => a -> T.Text
showText = T.pack . show

data LayerEvent
  = NewLayer Int Note
  | RemoveLayer Int

data Layer = Layer

data Note = Note deriving Show

data PlaybackState
  = Stopped
  | Playing
  | Paused
  deriving (Show, Eq)

