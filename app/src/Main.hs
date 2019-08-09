{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad (forever, guard, void)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.IntMap as M
import           Data.IORef
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Vector.Storable.Mutable as MV
import           Reflex
import           Reflex.Dom
import qualified SDL
import           System.Posix.Signals
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

handler :: IO ()
handler = do
  putStrLn "test"

main :: IO ()
main = do
  let layers = M.singleton 1 ( newLayer $ Pitch ANat 4 )
      tempo = 120
      vol = 10
  layerRef          <- newIORef layers
  tempoRef          <- newIORef tempo
  volumeRef         <- newIORef vol
  elapsedSamplesRef <- newIORef 0
  semaphore         <- newSemaphore

  SDL.initialize [SDL.InitAudio]
  (audioDevice, _audioSpec)
    <- SDL.openAudioDevice
     $ openDeviceSpec
     $ audioCallback semaphore layerRef tempoRef elapsedSamplesRef volumeRef

  let startPlayback = SDL.setAudioDevicePlaybackState audioDevice SDL.Play
      stopPlayback = SDL.setAudioDevicePlaybackState audioDevice SDL.Pause

      initState = JamState { _jamStLayersRef      = layerRef
                           , _jamStTempoRef       = tempoRef
                           , _jamStVolumeRef      = volumeRef
                           , _jamStElapsedSamples = elapsedSamplesRef
                           , _jamStSemaphore      = semaphore
                           , _jamStStartPlayback  = startPlayback
                           , _jamStStopPlayback   = stopPlayback
                           }

  installHandler keyboardSignal (Catch handler) Nothing

  css <- BS.readFile "app/css/styles.css"
  pure ()

  mainWidgetWithCss css $ el "div" $ do
    rec
      newLayerIdDyn <- holdDyn 1 $ maybe 1 (succ . fst) . M.lookupMax
                               <$> current layerMapDyn <@ newLayerEv
      randomNoteEv <- performEvent $ generateRandomNote <$ newLayerEv
      -- TODO need to perform layer adding and removing IO actions
      newLayerNoteDyn <- holdDyn ( Pitch ANat 4 ) randomNoteEv
      let newLayerEventDyn = NewLayer
                         <$> newLayerIdDyn
                         <*> ( LayerUI "1" "0" <$> newLayerNoteDyn )

          layerEvents = leftmost [ updated newLayerEventDyn, editLayerEvents ]
          initLayerMap = M.singleton 1 ( LayerUI "1" "0" ( Pitch ANat 4 ) )

      layerMapDyn <- foldDyn applyLayerEvent initLayerMap layerEvents

      let layerWidgetsDyn =
            ( \m -> M.traverseWithKey ( \i layerUI -> layerWidget initState i layerUI m ) m )
              <$> layerMapDyn

      -- create the layer widgets
      editLayerEvents <- switchHold never . fmap (leftmost . M.elems)
                     =<< dyn layerWidgetsDyn

      newLayerEv <- button "New Layer"

      -- Adds a new layer to the backend
      performEvent_ $ createNewLayer initState <$> current newLayerIdDyn
                                               <@> updated newLayerNoteDyn

      playbackStateDyn <- accum (const id)
                                Stopped
                                (leftmost [startEv, stopEv, pauseEv])

      let canPlayDyn  = fmap (/= Playing) playbackStateDyn
          canStopDyn  = fmap (/= Stopped) playbackStateDyn
          canPauseDyn = fmap (== Playing) playbackStateDyn

      startEv <- (Playing <$) <$> toggleButton canPlayDyn "Start"
      performEvent_ $ (liftIO $ startPlayback) <$ startEv

      let stopAction = liftIO $ do
            initState^.jamStStopPlayback
            writeIORef ( initState^.jamStElapsedSamples ) 0
            modifyIORef' ( initState^.jamStLayersRef ) ( fmap resetLayer )

      stopEv <- (Stopped <$) <$> toggleButton canStopDyn "Stop"
      performEvent_ $ stopAction <$ stopEv

      pauseEv <- (Paused <$) <$> toggleButton canPauseDyn "Pause"
      performEvent_ $ (liftIO $ stopPlayback) <$ pauseEv

    -- tempo
    text "Tempo: "
    tempoDyn <- numberInput (120.0 :: BPM) parseBpm bpmToText 1
    performEvent_ $ changeTempo initState <$> updated tempoDyn

    -- volume
    text "Vol.: "
    volumeDyn <- numberInput (5.0 :: Vol) parseVol volToText 0.2
    performEvent_ $ changeVol initState <$> updated volumeDyn

    pure ()

layerWidget :: (MonadHold t m, MonadFix m, DomBuilder t m, PerformEvent t m, MonadIO (Performable m))
            => JamState -> Int -> LayerUI -> M.IntMap LayerUI -> m (Event t LayerEvent)
layerWidget st layerId layerUI layerMap = el "div" $ do
  el "div" . text $ "Layer " <> showText layerId

  let validateBeatCode t = do
        let beatCodes = _layerUIBeatCode <$> layerMap
            beatCodes' = M.insert layerId t beatCodes
        -- reparse all layers
        -- TODO should only fail if the target layer fails to parse,
        -- otherwise should use the existing beatcode for that layer.
        -- we don't want to mark the current layer invalid because a
        -- different layer was invalid
        parsedCodes <- M.traverseWithKey (\i -> parseBeat i beatCodes') beatCodes'
        pure (parsedCodes, t)

  beatCodeInput <- textFieldInput (_layerUIBeatCode layerUI)
                                  validateBeatCode
                                  (const never)

  let beatCodeEv = fmap (liftIO . applyLayerBeatChange st . fst) beatCodeInput
  performEvent_ beatCodeEv
  let changeBeatCodeEv = ffor beatCodeInput $ \(_, txt) ->
        ChangeLayer layerId (layerUI & layerUIBeatCode .~ txt)

  pitchInput <- textFieldInput (pitchText $ _layerUIPitch layerUI)
                               parsePitch
                               (const never)

  let pitchEv = fmap (liftIO . applyLayerSourceChange st layerId) pitchInput
  performEvent_ pitchEv
  let changePitchEv = ffor pitchInput $ \n ->
        ChangeLayer layerId (layerUI & layerUIPitch .~ n)

      validateOffset t = do
        offset <- parseOffset t
        pure (offset, t)
  offsetInput <- textFieldInput (_layerUIOffset layerUI)
                                validateOffset
                                (const never)

  let offsetEv = fmap (liftIO . applyLayerOffsetChange st layerId . fst) offsetInput
  performEvent_ offsetEv
  let changeOffsetEv = ffor offsetInput $ \(_, o) ->
        ChangeLayer layerId (layerUI & layerUIOffset .~ o)

  deleteEv <- ( RemoveLayer layerId <$ ) <$> button "X"
  performEvent_ $ deleteLayer st layerId <$ deleteEv

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

numberInput :: (MonadHold t m, MonadFix m, DomBuilder t m, Num r)
            => r -> (T.Text -> Maybe r) -> (r -> T.Text) -> r -> m (Dynamic t r)
numberInput startingValue validator toText delta = do
  rec
    let s ev =
          let upArrowEv   = ( + delta ) <$ ffilter ( == ( 38 :: Word ) ) ev
              downArrowEv = ( subtract delta ) <$ ffilter ( == ( 40 :: Word ) ) ev
              setVal cur f = toText <$> ( validator . toText ) new
                where new = f cur
           in fmapMaybe id $ setVal <$> current inputDyn
                                    <@> leftmost [upArrowEv, downArrowEv]

    input <- textFieldInput ( toText startingValue ) validator s
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

applyLayerEvent :: LayerEvent -> M.IntMap LayerUI -> M.IntMap LayerUI
applyLayerEvent (NewLayer i l) = M.insert i l
applyLayerEvent (RemoveLayer i) = M.delete i
applyLayerEvent (ChangeLayer i l) = at i . _Just .~ l

createNewLayer :: MonadIO m => JamState -> Int -> Pitch -> m ()
createNewLayer st idx pitch = liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
  elapsedSamples <- readIORef ( st^.jamStElapsedSamples )
  tempo <- readIORef ( st^.jamStTempoRef )
  let elapsedCells =
        numSamplesToCellValue tempo elapsedSamples
      layer = newLayer pitch

  void $ modifyIORef' ( st^.jamStLayersRef )
                      ( fmap ( syncLayer elapsedCells )
                      . ( at idx ?~ layer )
                      )

deleteLayer :: MonadIO m => JamState -> Int -> m ()
deleteLayer st idx = liftIO $
  modifyIORef' ( st^.jamStLayersRef )
               ( sans idx )

changeTempo :: MonadIO m => JamState -> BPM -> m ()
changeTempo st newTempo = liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
  currentTempo <- readIORef $ st^.jamStTempoRef
  let ratio = getBPM $ currentTempo / newTempo

  modifyIORef' ( st^.jamStElapsedSamples ) ( * ratio )
  writeIORef ( st^.jamStTempoRef ) newTempo

changeVol :: MonadIO m => JamState -> Vol -> m ()
changeVol st newVol = liftIO $ do
  currentVol <- readIORef $ st^.jamStVolumeRef
  writeIORef ( st^.jamStVolumeRef ) newVol

generateRandomNote :: MonadIO m => m Pitch
generateRandomNote = liftIO randomIO

showText :: Show a => a -> T.Text
showText = T.pack . show

openDeviceSpec :: (forall s. SDL.AudioFormat s -> MV.IOVector s -> IO ()) -> SDL.OpenDeviceSpec
openDeviceSpec callback = SDL.OpenDeviceSpec
  { SDL.openDeviceFreq = SDL.Mandate 44100
    -- ^ The output audio frequency in herts.
  , SDL.openDeviceFormat = SDL.Mandate SDL.FloatingNativeAudio
    -- ^ The format of audio that will be sampled from the output buffer.
  , SDL.openDeviceChannels = SDL.Mandate SDL.Stereo
    -- ^ The amount of audio channels.
  , SDL.openDeviceSamples = 1024
    -- ^ Output audio buffer size in samples. This should be a power of 2.
  , SDL.openDeviceCallback = callback
    -- ^ A callback to invoke whenever new sample data is required. The callback
    -- will be passed a single 'MV.MVector' that must be filled with audio data.
  , SDL.openDeviceUsage = SDL.ForPlayback
    -- ^ How you intend to use the opened 'AudioDevice' - either for outputting
    -- or capturing audio.
  , SDL.openDeviceName = Nothing
    -- ^ The name of the 'AudioDevice' that should be opened. If 'Nothing',
    -- any suitable 'AudioDevice' will be used.
  }

