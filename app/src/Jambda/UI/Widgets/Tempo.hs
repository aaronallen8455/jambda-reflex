{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Jambda.UI.Widgets.Tempo
  ( tempoWidget
  ) where

import           Control.Lens
import           Control.Monad (join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef (writeIORef, modifyIORef', readIORef)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.UnixTime (UnixTime(..), getUnixTime)
import           Foreign.C.Types (CTime(..))
import           Reflex.Dom

import           Jambda.Data
import           Jambda.Types
import           Jambda.UI.Widgets.NumberInput (numberInput)

tempoWidget :: JambdaUI t m => JamState -> m ()
tempoWidget st = mdo
  let initTempoWidget =
        numberInput (Just "Tempo")
                    (Just "tempo-input")
                    120
                    parseBpm
                    bpmToText
                    1

  tempoDyn <- fmap join . widgetHold initTempoWidget
                        . ffor tempoTapBpmEv $ \bpm ->
    numberInput (Just "Tempo")
                (Just "tempo-input")
                bpm
                parseBpm
                bpmToText
                1

  performEvent_ $ changeTempo st <$> leftmost [updated tempoDyn, tempoTapBpmEv]
  tempoTapEv <- button "Tap"
  timeEv <- performEvent $ (pure . unixTimeToSecs =<< liftIO getUnixTime)
                             <$ tempoTapEv
  tempoTapBpm <- fmap (fmap secToBPM . tempoTapAvg)
             <$> foldDyn buildTimeList (pure 0) timeEv
  let tempoTapBpmEv = fmapMaybe id $ updated tempoTapBpm
  pure ()

changeTempo :: MonadIO m => JamState -> BPM -> m ()
changeTempo st newTempo = liftIO . signalSemaphore ( st^.jamStSemaphore ) $ do
  currentTempo <- readIORef $ st^.jamStTempoRef
  let ratio = getBPM $ currentTempo / newTempo

  modifyIORef' ( st^.jamStElapsedSamples ) ( * ratio )
  writeIORef ( st^.jamStTempoRef ) newTempo

cTimeToSecs :: CTime -> Int
cTimeToSecs (CTime x) = fromIntegral x

unixTimeToSecs :: UnixTime -> Double
unixTimeToSecs ut = (fromIntegral . cTimeToSecs $ utSeconds ut) + (fromIntegral (utMicroSeconds ut) / 1000000)

buildTimeList :: Double -> NonEmpty Double -> NonEmpty Double
buildTimeList x (h :| xs)
  | x - h >= 2 = pure x
  | otherwise  = x :| take 6 (h : xs)

tempoTapAvg :: NonEmpty Double -> Maybe Sec
tempoTapAvg (_ :| []) = Nothing
tempoTapAvg (h :| xs) = Just . Sec . (/ fromIntegral (length xs)) . sum
                      $ zipWith (-) (h:xs) xs
