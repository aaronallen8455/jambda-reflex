{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad (forever, guard, void)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bifunctor (second)
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
    { _layerUIBeatCode :: InputState T.Text
    , _layerUIOffset :: InputState T.Text
    , _layerUIPitch :: InputState Pitch
    }

mkNewLayerUI :: T.Text -> T.Text -> Pitch -> LayerUI
mkNewLayerUI beat offset pitch =
  LayerUI
    { _layerUIBeatCode = InputState beat Nothing
    , _layerUIOffset   = InputState offset Nothing
    , _layerUIPitch    = InputState pitch Nothing
    }

data InputState a =
  InputState
    { _inpValid :: a
    , _inpInvalid :: Maybe T.Text
    } deriving (Show, Functor)

data PlaybackState
  = Stopped
  | Playing
  | Paused
  deriving (Show, Eq)

newtype BeatCode = BeatCode T.Text deriving (Show, Eq)

makeLenses ''LayerUI
makeLenses ''InputState

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
                         <*> ( mkNewLayerUI "1" "0" <$> newLayerNoteDyn )

          layerEvents = leftmost [ updated newLayerEventDyn, editLayerEvents ]
          initLayerMap = M.singleton 1 ( mkNewLayerUI "1" "0" ( Pitch ANat 4 ) )

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

  -- Beatcode input
  let validateBeatCode :: T.Text -> Maybe (M.IntMap (NonEmpty Cell'), T.Text)
      validateBeatCode t = do
        let beatCodes' = _inpValid . _layerUIBeatCode <$> layerMap
            beatCodes'' = M.insert layerId t beatCodes'
            parse i = parseBeat i beatCodes'
        parsedCodes <- M.traverseWithKey parse beatCodes''
        pure (parsedCodes, t)

  beatCodeInput <- textFieldInput (_layerUIBeatCode layerUI)
                                  validateBeatCode
                                  (const never)

  let beatCodeEv = fforMaybe beatCodeInput $ \case
        Left _ -> Nothing
        Right (v, _) -> Just . liftIO $ applyLayerBeatChange st v
  performEvent_ beatCodeEv
  let changeBeatCodeEv = ffor beatCodeInput $ \case
        Left inv        -> ChangeLayer layerId (layerUI & layerUIBeatCode . inpInvalid .~ Just inv)
        Right (_, txt)  -> ChangeLayer layerId (layerUI & layerUIBeatCode . inpValid .~ txt
                                                        & layerUIBeatCode . inpInvalid .~ Nothing
                                               )

  -- Pitch Input
  pitchInput <- textFieldInput (pitchText <$> _layerUIPitch layerUI)
                               parsePitch
                               (const never)

  let pitchEv = fforMaybe pitchInput $
        either (const Nothing)
               (Just . liftIO . applyLayerSourceChange st layerId)

  performEvent_ pitchEv
  let changePitchEv = ffor pitchInput $ \case
        Left inv -> ChangeLayer layerId (layerUI & layerUIPitch . inpInvalid .~ Just inv)
        Right n  -> ChangeLayer layerId (layerUI & layerUIPitch . inpValid .~ n
                                                 & layerUIPitch . inpInvalid .~ Nothing
                                        )

  -- Offset input
  let validateOffset t = do
        offset <- parseOffset t
        pure (offset, t)
  offsetInput <- textFieldInput (_layerUIOffset layerUI)
                                validateOffset
                                (const never)

  let offsetEv = fforMaybe offsetInput $
        either (const Nothing)
               (Just . liftIO . applyLayerOffsetChange st layerId . fst)
  performEvent_ offsetEv
  let changeOffsetEv = ffor offsetInput $ \case
        Left inv     -> ChangeLayer
                          layerId
                          (layerUI & layerUIOffset . inpInvalid .~ Just inv)
        Right (_, t) -> ChangeLayer
                          layerId
                          (layerUI & layerUIOffset . inpInvalid .~ Nothing
                                   & layerUIOffset . inpValid .~ t
                          )

  -- Delete button
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
               => InputState T.Text
               -> (T.Text -> Maybe a)
               -> (Event t Word -> Event t T.Text)
               -> m (Event t (Either T.Text a))
textFieldInput inpState validator mkSetValEv = do
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

    let validate v    = maybe (Left v) Right $ validator v
        eitherValEv   = validate <$> valueEv
        invalidAttrEv = ffor eitherValEv $ either (const invalidAttrs)
                                                  (const validAttrs)
        editingAttrEv = editingAttrs <$ editingEv
        attrEv        = leftmost [invalidAttrEv, editingAttrEv]

  pure eitherValEv

numberInput :: (MonadHold t m, MonadFix m, DomBuilder t m, Num r)
            => r
            -> (T.Text -> Maybe r)
            -> (r -> T.Text)
            -> r
            -> m (Dynamic t r)
numberInput initialVal validator toText delta = do
  rec
    let s ev =
          let upArrowEv   = ( + delta ) <$ ffilter ( == ( 38 :: Word ) ) ev
              downArrowEv = ( subtract delta ) <$ ffilter ( == ( 40 :: Word ) ) ev
              setVal cur f = toText <$> ( validator . toText ) new
                where new = f cur
           in fmapMaybe id $ setVal <$> current inputDyn
                                    <@> leftmost [upArrowEv, downArrowEv]

    input <- textFieldInput ( InputState (toText initialVal) Nothing ) validator s
    let validInput = fforMaybe input $ either (const Nothing) Just
    inputDyn <- holdDyn initialVal validInput

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

