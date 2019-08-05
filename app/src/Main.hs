{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens
import           Control.Monad (guard)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.IntMap as M
import           Data.List.NonEmpty (NonEmpty)
import           Reflex
import           Reflex.Dom
import           System.Random (randomIO)
import           Text.Read (readMaybe)

import           Jambda.Data
import           Jambda.Types

data LayerEvent
  = NewLayer Int LayerUI
  | RemoveLayer Int
  | ChangeLayer Int LayerUI

data LayerUI =
  LayerUI
    { _layerUIBeatCode :: T.Text
    , _layerUIOffset :: T.Text
    , _layerUIPitch :: Pitch
    }

data PlaybackState
  = Stopped
  | Playing
  | Paused
  deriving (Show, Eq)

newtype BeatCode = BeatCode T.Text deriving (Show, Eq)

makeLenses ''LayerUI

main :: IO ()
main = do
  -- set up the backend stuff
  -- - MVar with the layers models
  -- - MVar with the tempo
  -- - MVar with the volume
  -- will supply them to the network via ReaderT

  css <- BS.readFile "app/css/styles.css"
  mainWidgetWithCss css $ el "div" $ do
    rec
      newLayerIdDyn <- count newLayerEv
      randomNoteEv <- performEvent $ generateRandomNote <$ newLayerEv
      -- TODO need to perform layer adding and removing IO actions
      newLayerNoteDyn <- holdDyn ( Pitch ANat ( Octave 4 ) ) randomNoteEv
      let newLayerEventDyn = NewLayer
                         <$> newLayerIdDyn
                         <*> ( LayerUI "1" "" <$> newLayerNoteDyn )

          layerEvents = leftmost [ updated newLayerEventDyn, editLayerEvents ]

      layerMapDyn <- foldDyn applyLayerEvent mempty layerEvents

      let layerWidgetsDyn = ( \m -> M.traverseWithKey ( \i layerUI -> layerWidget i layerUI m ) m )
                        <$> layerMapDyn

      -- create the layer widgets
      editLayerEvents <- switchHold never . fmap (leftmost . M.elems)
                     =<< dyn layerWidgetsDyn

      newLayerEv <- button "New Layer"

      -- Adds a new layer to the backend
      performEvent_ $ createNewLayer <$> current newLayerIdDyn
                                     <@> updated newLayerNoteDyn

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
    tempoDyn <- numberInput (120.0 :: Double) (> 0)
    performEvent_ $ (liftIO $ putStrLn "Changing tempo") <$ updated tempoDyn
    -- volume
    text "Vol.: "
    volumeDyn <- numberInput (5.0 :: Double) (\x -> x >= 0 && x <= 10)
    performEvent_ $ (liftIO $ putStrLn "Changing volume") <$ updated volumeDyn

    pure ()

layerWidget :: (MonadHold t m, MonadFix m, DomBuilder t m, PerformEvent t m, MonadIO (Performable m))
            => Int -> LayerUI -> M.IntMap LayerUI -> m (Event t LayerEvent)
layerWidget layerId layerUI layerMap = el "div" $ do
  el "div" . text $ "Layer " <> showText layerId

  let validateBeatCode t = do
        let beatCodes = _layerUIBeatCode <$> layerMap
            beatCodes' = M.insert layerId t beatCodes
        code <- parseBeat layerId beatCodes' t
        pure (code, t)

  beatCodeInput <- textFieldInput (_layerUIBeatCode layerUI)
                                  validateBeatCode
                                  (const never)

  let beatCodeEv = fmap (updateLayerBeatCode layerId . fst) beatCodeInput
  performEvent_ beatCodeEv
  let changeBeatCodeEv = ffor beatCodeInput $ \(_, txt) ->
        ChangeLayer layerId (layerUI & layerUIBeatCode .~ txt)

  pitchInput <- textFieldInput (pitchText $ _layerUIPitch layerUI)
                               parsePitch
                               (const never)

  let pitchEv = fmap (updateLayerNote layerId) pitchInput
  performEvent_ pitchEv
  let changePitchEv = ffor pitchInput $ \n ->
        ChangeLayer layerId (layerUI & layerUIPitch .~ n)

  offsetInput <- textFieldInput (_layerUIOffset layerUI)
                                Just
                                (const never)

  let offsetEv = fmap (updateLayerOffset layerId) offsetInput
  performEvent_ offsetEv
  let changeOffsetEv = ffor offsetInput $ \o ->
        ChangeLayer layerId (layerUI & layerUIOffset .~ o)

  deleteEv <- ( RemoveLayer layerId <$ ) <$> button "X"
  performEvent_ $ deleteLayer layerId <$ deleteEv

  pure $ leftmost [ deleteEv
                  , changeBeatCodeEv
                  , changePitchEv
                  , changeOffsetEv
                  ]

-- | an input field that only updates its value on blur or pressing enter, and
-- only if the text contents are successfully validated.
textFieldInput :: (MonadHold t m, MonadFix m, DomBuilder t m)
               => T.Text
               -> (T.Text -> Maybe a)
               -> (Event t Word -> Event t T.Text)
               -> m (Event t a)
textFieldInput initialText validator mkSetValEv = do
  rec
    input <- inputElement $
      def & inputElementConfig_initialValue .~ initialText
          & inputElementConfig_setValue .~ setValEv
          & inputElementConfig_elementConfig
              %~ elementConfig_modifyAttributes .~ attrEv

    let element'   = _inputElement_element input
        keydownEv  = domEvent Keydown element'
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

    let mbValEv = validator <$> valueEv
        invalidAttrs  = "class" =: Just "invalid-field"
        editingAttrs  = "class" =: Just "edited-field"
        validAttrs    = "class" =: Nothing
        invalidAttrEv = ffor mbValEv $ maybe invalidAttrs
                                             (const validAttrs)
        editingAttrEv = editingAttrs <$ editingEv
        attrEv        = leftmost [invalidAttrEv, editingAttrEv]

  pure $ fmapMaybe id mbValEv

numberInput :: (MonadHold t m, MonadFix m, DomBuilder t m, Show a, Read a, Enum a)
            => a -> (a -> Bool) -> m (Dynamic t a)
numberInput startingValue validator = do
  rec
    let validate v = do
          n <- readMaybe $ T.unpack v
          guard $ validator n
          pure n
        s ev =
          let upArrowEv   = succ <$ ffilter ( == ( 38 :: Word ) ) ev
              downArrowEv = pred <$ ffilter ( == ( 40 :: Word ) ) ev
              setVal cur f
                | validator new = Just $ showText new
                | otherwise = Nothing
                where new = f cur
              setValEv = fmapMaybe id $ setVal <$> current inputDyn
                                               <@> leftmost [upArrowEv, downArrowEv]
           in setValEv

    input <- textFieldInput ( showText startingValue ) validate s
    inputDyn <- holdDyn startingValue input

  pure inputDyn

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

applyLayerEvent :: LayerEvent -> M.IntMap LayerUI -> M.IntMap LayerUI
applyLayerEvent (NewLayer i l) = M.insert i l
applyLayerEvent (RemoveLayer i) = M.delete i
applyLayerEvent (ChangeLayer i l) = at i . _Just .~ l

createNewLayer :: MonadIO m => Int -> Pitch -> m ()
createNewLayer idx pitch = liftIO $
  putStrLn $ "Creating new layer " <> show idx <> T.unpack (pitchText pitch)

deleteLayer :: MonadIO m => Int -> m ()
deleteLayer _idx = liftIO $
  putStrLn "Deleting Layer"

updateLayerBeatCode :: MonadIO m => Int -> NonEmpty Cell' -> m ()
updateLayerBeatCode _layerId _newBeatCode = liftIO $
  putStrLn "Update beatcode of Layer."

updateLayerNote :: MonadIO m => Int -> Pitch -> m ()
updateLayerNote layerId newNote = liftIO $
  putStrLn $ "Update note of Layer " <> show layerId <> " to " <> (T.unpack $ pitchText newNote)

updateLayerOffset :: MonadIO m => Int -> T.Text -> m ()
updateLayerOffset layerId offset = liftIO $
  putStrLn $ "Update offset of Layer " <> show layerId <> " to " <> show offset

generateRandomNote :: MonadIO m => m Pitch
generateRandomNote = liftIO randomIO

showText :: Show a => a -> T.Text
showText = T.pack . show

