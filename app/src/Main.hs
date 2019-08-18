{-# LANGUAGE RankNTypes #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.IntMap as M
import           Data.IORef
import qualified Data.Vector.Storable.Mutable as MV
import           Reflex.Dom
import qualified SDL

import           Jambda.Data
import           Jambda.Types
import           Jambda.UI (rootWidget)

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

  css <- BS.readFile "app/css/styles.css"

  mainWidgetWithCss css (rootWidget initState)

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

