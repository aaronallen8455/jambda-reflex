{-# LANGUAGE TemplateHaskell #-}
module Jambda.Types.SoundSource.Wav where

import           Data.Aeson.TH
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
    , _wavPtr     :: !(Ptr Word8)
    , _wavIdx     :: Int
    }

makeLenses ''Wav

newtype PersistedWav = PersistedWav { persistedWavFile :: T.Text }

wavDir :: FilePath
wavDir = "app/wav/"

wavFiles :: V.Vector (FilePath, T.Text)
wavFiles = V.fromList
  [ (wavDir <> "crash1_edge_v10.wav"         , "Crash Edge V10"          )
  , (wavDir <> "crash1_edge_v5.wav"          , "Crash Edge V5"           )
  , (wavDir <> "crash1_edge_v8.wav"          , "Crash Edge V8"           )
  , (wavDir <> "floortom_v11.wav"            , "FloorTom V11"            )
  , (wavDir <> "floortom_v16.wav"            , "FloorTom V16"            )
  , (wavDir <> "floortom_v6.wav"             , "FloorTom V6"             )
  , (wavDir <> "hihat_closed_center_v4.wav"  , "HiHat Closed Center V4"  )
  , (wavDir <> "hihat_closed_center_v7.wav"  , "HiHat Closed Center V7"  )
  , (wavDir <> "hihat_closed_center_v10.wav" , "HiHat Closed Center V10" )
  , (wavDir <> "hihat_closed_edge_v7.wav"    , "HiHat Closed Edge V7"    )
  , (wavDir <> "hihat_closed_edge_v10.wav"   , "HiHat Closed Edge V10"   )
  , (wavDir <> "hihat_half_center_v4.wav"    , "HiHat Half Center V4"    )
  , (wavDir <> "hihat_half_center_v7.wav"    , "HiHat Half Center V7"    )
  , (wavDir <> "hihat_half_center_v10.wav"   , "HiHat Half Center V10"   )
  , (wavDir <> "hihat_half_edge_v7.wav"      , "HiHat Half Edge V7"      )
  , (wavDir <> "hihat_half_edge_v10.wav"     , "HiHat Half Edge V10"     )
  , (wavDir <> "hihat_open_center_v4.wav"    , "HiHat Open Center V4"    )
  , (wavDir <> "hihat_open_center_v7.wav"    , "HiHat Open Center V7"    )
  , (wavDir <> "hihat_open_center_v10.wav"   , "HiHat Open Center V10"   )
  , (wavDir <> "hihat_open_edge_v7.wav"      , "HiHat Open Edge V7"      )
  , (wavDir <> "hihat_open_edge_v10.wav"     , "HiHat Open Edge V10"     )
  , (wavDir <> "hihat_pedal_v3.wav"          , "HiHat Pedal V3"          )
  , (wavDir <> "hihat_pedal_v5.wav"          , "HiHat Pedal V5"          )
  , (wavDir <> "kick_v7.wav"                 , "Kick V7"                 )
  , (wavDir <> "kick_v11.wav"                , "Kick V11"                )
  , (wavDir <> "kick_v16.wav"                , "Kick V16"                )
  , (wavDir <> "racktom_v6.wav"              , "RackTom V6"              )
  , (wavDir <> "racktom_v11.wav"             , "RackTom V11"             )
  , (wavDir <> "racktom_v16.wav"             , "RackTom V16"             )
  , (wavDir <> "ride_bell_v5.wav"            , "Ride Bell V5"            )
  , (wavDir <> "ride_bell_v8.wav"            , "Ride Bell V8"            )
  , (wavDir <> "ride_bell_v10.wav"           , "Ride Bell V10"           )
  , (wavDir <> "ride_center_v5.wav"          , "Ride Center V5"          )
  , (wavDir <> "ride_center_v6.wav"          , "Ride Center V6"          )
  , (wavDir <> "ride_center_v8.wav"          , "Ride Center V8"          )
  , (wavDir <> "ride_center_v10.wav"         , "Ride Center V10"         )
  , (wavDir <> "ride_edge_v4.wav"            , "Ride Edge V4"            )
  , (wavDir <> "ride_edge_v7.wav"            , "Ride Edge V7"            )
  , (wavDir <> "ride_edge_v10.wav"           , "Ride Edge V10"           )
  , (wavDir <> "snare_center_v6.wav"         , "Snare Center V6"         )
  , (wavDir <> "snare_center_v11.wav"        , "Snare Center V11"        )
  , (wavDir <> "snare_center_v16.wav"        , "Snare Center V16"        )
  , (wavDir <> "snare_edge_v6.wav"           , "Snare Edge V6"           )
  , (wavDir <> "snare_edge_v11.wav"          , "Snare Edge V11"          )
  , (wavDir <> "snare_edge_v16.wav"          , "Snare Edge V16"          )
  , (wavDir <> "snare_rim_v6.wav"            , "Snare Rim V6"            )
  , (wavDir <> "snare_rim_v11.wav"           , "Snare Rim V11"           )
  , (wavDir <> "snare_rim_v16.wav"           , "Snare Rim V16"           )
  , (wavDir <> "snare_xstick_v6.wav"         , "Snare XStick V6"         )
  , (wavDir <> "snare_xstick_v11.wav"        , "Snare XStick V11"        )
  , (wavDir <> "snare_xstick_v16.wav"        , "Snare XStick V16"        )
  , (wavDir <> "silence.wav"                 , "Silence"                 )
  ]

$(deriveJSON defaultOptions ''PersistedWav)
