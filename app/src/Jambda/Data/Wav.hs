module Jambda.Data.Wav
  ( loadWavFiles
  , loadWavFile
  ) where

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
loadWavFiles = traverse mkWav wavFiles
  where
    mkWav (fp, label, hh) = do
      samples <- loadWavFile fp
      pure Wav { _wavLabel   = label
               , _wavSamples = V.toList samples
               , _wavHighHat = hh
               }

-- | Extract the samples from a wav file
loadWavFile :: FilePath -> IO (V.Vector Sample)
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
                  2
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
  let samples = SV.unsafeCast
              $ SV.unsafeFromForeignPtr0 bufferFPtr ( fromIntegral len )

  free audioCVTPtr
  freeWAV cvtBufferPtr
  free bufferPtrPtr
  free lenPtr
  free specPtr

  pure $ SV.convert samples

