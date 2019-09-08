{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.SoundSource.Wav where

import           Control.Lens
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word8)
import           Foreign.Ptr (Ptr)

import           Jambda.Types.Newtypes (Sample)

data Wav =
  Wav
    { _wavLabel   :: !T.Text
    , _wavSamples :: ![Sample]
    , _wavHighHat :: !(Maybe HighHatStatus)
    , _wavPtr     :: !(Ptr Word8)
    , _wavIdx     :: Int
    }

data HighHatStatus
  = HHOpen
  | HHClose

makeLenses ''Wav
makePrisms ''HighHatStatus

wavDir :: FilePath
wavDir = "app/wav/"

wavFiles :: V.Vector (FilePath, T.Text, Maybe HighHatStatus)
wavFiles = V.fromList
  [ (wavDir <> "crash1_edge_v10.wav"         , "Crash Edge V10"          , Nothing     )
  , (wavDir <> "crash1_edge_v5.wav"          , "Crash Edge V5"           , Nothing     )
  , (wavDir <> "crash1_edge_v8.wav"          , "Crash Edge V8"           , Nothing     )
  , (wavDir <> "floortom_v11.wav"            , "FloorTom V11"            , Nothing     )
  , (wavDir <> "floortom_v16.wav"            , "FloorTom V16"            , Nothing     )
  , (wavDir <> "floortom_v6.wav"             , "FloorTom V6"             , Nothing     )
  , (wavDir <> "hihat_closed_center_v4.wav"  , "HiHat Closed Center V4"  , Nothing     )
  , (wavDir <> "hihat_closed_center_v7.wav"  , "HiHat Closed Center V7"  , Nothing     )
  , (wavDir <> "hihat_closed_center_v10.wav" , "HiHat Closed Center V10" , Nothing     )
  , (wavDir <> "hihat_closed_edge_v7.wav"    , "HiHat Closed Edge V7"    , Nothing     )
  , (wavDir <> "hihat_closed_edge_v10.wav"   , "HiHat Closed Edge V10"   , Nothing     )
  , (wavDir <> "hihat_half_center_v4.wav"    , "HiHat Half Center V4"    , Just HHOpen )
  , (wavDir <> "hihat_half_center_v7.wav"    , "HiHat Half Center V7"    , Just HHOpen )
  , (wavDir <> "hihat_half_center_v10.wav"   , "HiHat Half Center V10"   , Just HHOpen )
  , (wavDir <> "hihat_half_edge_v7.wav"      , "HiHat Half Edge V7"      , Just HHOpen )
  , (wavDir <> "hihat_half_edge_v10.wav"     , "HiHat Half Edge V10"     , Just HHOpen )
  , (wavDir <> "hihat_open_center_v4.wav"    , "HiHat Open Center V4"    , Just HHOpen )
  , (wavDir <> "hihat_open_center_v7.wav"    , "HiHat Open Center V7"    , Just HHOpen )
  , (wavDir <> "hihat_open_center_v10.wav"   , "HiHat Open Center V10"   , Just HHOpen )
  , (wavDir <> "hihat_open_edge_v7.wav"      , "HiHat Open Edge V7"      , Just HHOpen )
  , (wavDir <> "hihat_open_edge_v10.wav"     , "HiHat Open Edge V10"     , Just HHOpen )
  , (wavDir <> "hihat_pedal_v3.wav"          , "HiHat Pedal V3"          , Just HHClose)
  , (wavDir <> "hihat_pedal_v5.wav"          , "HiHat Pedal V5"          , Just HHClose)
  , (wavDir <> "kick_v7.wav"                 , "Kick V7"                 , Nothing     )
  , (wavDir <> "kick_v11.wav"                , "Kick V11"                , Nothing     )
  , (wavDir <> "kick_v16.wav"                , "Kick V16"                , Nothing     )
  , (wavDir <> "racktom_v6.wav"              , "RackTom V6"              , Nothing     )
  , (wavDir <> "racktom_v11.wav"             , "RackTom V11"             , Nothing     )
  , (wavDir <> "racktom_v16.wav"             , "RackTom V16"             , Nothing     )
  , (wavDir <> "ride_bell_v5.wav"            , "Ride Bell V5"            , Nothing     )
  , (wavDir <> "ride_bell_v8.wav"            , "Ride Bell V8"            , Nothing     )
  , (wavDir <> "ride_bell_v10.wav"           , "Ride Bell V10"           , Nothing     )
  , (wavDir <> "ride_center_v5.wav"          , "Ride Center V5"          , Nothing     )
  , (wavDir <> "ride_center_v6.wav"          , "Ride Center V6"          , Nothing     )
  , (wavDir <> "ride_center_v8.wav"          , "Ride Center V8"          , Nothing     )
  , (wavDir <> "ride_center_v10.wav"         , "Ride Center V10"         , Nothing     )
  , (wavDir <> "ride_edge_v4.wav"            , "Ride Edge V4"            , Nothing     )
  , (wavDir <> "ride_edge_v7.wav"            , "Ride Edge V7"            , Nothing     )
  , (wavDir <> "ride_edge_v10.wav"           , "Ride Edge V10"           , Nothing     )
  , (wavDir <> "snare_center_v6.wav"         , "Snare Center V6"         , Nothing     )
  , (wavDir <> "snare_center_v11.wav"        , "Snare Center V11"        , Nothing     )
  , (wavDir <> "snare_center_v16.wav"        , "Snare Center V16"        , Nothing     )
  , (wavDir <> "snare_edge_v6.wav"           , "Snare Edge V6"           , Nothing     )
  , (wavDir <> "snare_edge_v11.wav"          , "Snare Edge V11"          , Nothing     )
  , (wavDir <> "snare_edge_v16.wav"          , "Snare Edge V16"          , Nothing     )
  , (wavDir <> "snare_rim_v6.wav"            , "Snare Rim V6"            , Nothing     )
  , (wavDir <> "snare_rim_v11.wav"           , "Snare Rim V11"           , Nothing     )
  , (wavDir <> "snare_rim_v16.wav"           , "Snare Rim V16"           , Nothing     )
  , (wavDir <> "snare_xstick_v6.wav"         , "Snare XStick V6"         , Nothing     )
  , (wavDir <> "snare_xstick_v11.wav"        , "Snare XStick V11"        , Nothing     )
  , (wavDir <> "snare_xstick_v16.wav"        , "Snare XStick V16"        , Nothing     )
  , (wavDir <> "silence.wav"                 , "Silence"                 , Nothing     )
  ]
