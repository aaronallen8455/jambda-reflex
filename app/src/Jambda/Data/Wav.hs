module Jambda.Data.Wav
  ( loadWavFiles
  , loadWavFile
  ) where

import           Control.Lens (itraverse)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import           Data.Word (Word8, Word32)
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Marshal hiding (throwIfNeg_)
import           Foreign.Ptr
import           Foreign.Storable

import           SDL.Internal.Exception
import           SDL.Raw.Audio
import           SDL.Raw.Enum
import           SDL.Raw.Types

import           Jambda.Types

loadWavFiles :: IO (V.Vector Wav)
loadWavFiles = itraverse mkWav wavFiles
  where
    mkWav i (fp, label) = do
      (samples, ptr) <- loadWavFile fp
      pure Wav { _wavLabel   = label
               , _wavSamples = V.toList samples
               , _wavPtr = ptr
               , _wavIdx = i
               }

-- | Extract the samples from a wav file
loadWavFile :: FilePath -> IO (V.Vector Sample, Ptr Word8)
loadWavFile filePath = do
  -- Load the wav file into memory
  fpath        <- newCString filePath
  bufferPtrPtr <- malloc :: IO (Ptr (Ptr Word8))
  lenPtr       <- malloc :: IO (Ptr Word32)
  specPtr      <- malloc :: IO (Ptr AudioSpec)
  resultSpec   <- loadWAV fpath specPtr bufferPtrPtr lenPtr

  spec <- peek resultSpec

  -- Convert to 32 bit floats
  audioCVTPtr <- malloc
  throwIfNeg_ "loadWav" "buildAudioCVT" $
    buildAudioCVT audioCVTPtr
                  (audioSpecFormat spec)
                  (audioSpecChannels spec)
                  (audioSpecFreq spec)
                  SDL_AUDIO_F32SYS -- Native floating point
                  1
                  44100

  len          <- peek lenPtr
  bufferPtr    <- peek bufferPtrPtr
  audioCVT     <- peek audioCVTPtr
  cvtBufferPtr <- mallocBytes . fromIntegral $ fromIntegral len * audioCVTLenMult audioCVT
  copyBytes cvtBufferPtr bufferPtr (fromIntegral len)
  freeWAV bufferPtr
  poke audioCVTPtr audioCVT { audioCVTBuf = cvtBufferPtr
                            , audioCVTLen = fromIntegral len
                            }

  throwIfNeg_ "loadWav" "convertAudio" $ convertAudio audioCVTPtr

  bufferFPtr <- newForeignPtr_ cvtBufferPtr
  let samples = SV.convert
              . SV.unsafeCast
              $ SV.unsafeFromForeignPtr0 bufferFPtr ( fromIntegral len )

  finalizeForeignPtr bufferFPtr
  free audioCVTPtr
  free bufferPtrPtr
  free lenPtr
  free specPtr

  pure (samples, cvtBufferPtr)

