module Jambda.Data.Persistence
  ( persistBeat
  , recoverBeat
  , getSavedBeats
  , deleteSavedBeat
  ) where

import           Control.Arrow ((&&&))
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans (lift)
import           Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.IntMap as IM
import           Data.IORef (readIORef, writeIORef)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import           System.Directory (listDirectory, removeFile)

import           Jambda.Data.Layer
import           Jambda.Data.Parsers
import           Jambda.Types

savedFilePath :: FilePath
savedFilePath = "app/saved-beats/"

getSavedBeats :: IO [BeatFileName]
getSavedBeats = map (BeatFileName . T.pack) <$> listDirectory savedFilePath

persistBeat :: JamState -> BeatFileName -> IM.IntMap LayerUI -> IO ()
persistBeat jamSt (BeatFileName fileName) layers =
    BS.writeFile filePath =<< persistedBeatBytes
  where
    filePath = savedFilePath <> T.unpack fileName
    persistedBeatBytes = encode <$> persistedBeat
    persistedBeat =
      PersistedBeat
        <$> readIORef ( _jamStTempoRef jamSt )
        <*> pure persistedLayers'
    persistedLayers' = mkPersistedLayer <$> layers

mkPersistedLayer :: LayerUI -> PersistedLayerUI
mkPersistedLayer = layerUISoundSource . _SSWav %~ mkPersistedWav
  where
    mkPersistedWav = PersistedWav . _wavLabel

recoverBeat :: JamState -> BeatFileName -> IO (Maybe (BPM, IM.IntMap LayerUI))
recoverBeat jamSt (BeatFileName fileName) = runMaybeT $ do
  persistedBeat <- MaybeT getPersistedBeat

  -- put wav data in a Map
  let wavMap = M.fromList . map ( _wavLabel &&& id )
             . V.toList $ _jamStWavSources jamSt
      lookupWav (PersistedWav wavTitle) = M.lookup wavTitle wavMap

  -- Replace persisted wavs with actual wav data
  layerUIMap <- MaybeT . pure
              . traverse (layerUISoundSource . _SSWav %%~ lookupWav)
              $ persistedLayers persistedBeat

  -- create new Layer objects to replace existing ones
  let newLayers = newLayer <$> ( _layerUISoundSource <$> layerUIMap )
  lift $ writeIORef ( _jamStLayersRef jamSt ) newLayers

  -- Parse the persisted beatcodes and apply to layers.
  let beatCodes = _inpValid . _layerUIBeatCode <$> persistedLayers persistedBeat
      pBeat idx txt = parseBeat idx beatCodes ( _jamStWavSources jamSt ) txt

  parsedBeatCodes <- MaybeT . pure $ IM.traverseWithKey pBeat beatCodes
  lift $ applyLayerBeatChange jamSt parsedBeatCodes

  -- Parse and apply offsets
  let changeOffset idx layerUI
        = MaybeT . traverse (applyLayerOffsetChange jamSt idx)
        $ parseOffset (layerUI^.layerUIOffset.inpValid)

  _ <- IM.traverseWithKey changeOffset layerUIMap

  -- apply Vol
  let changeVol idx layerUI
        = lift $ applyLayerVolChange jamSt idx (layerUI^.layerUIVol)

  _ <- IM.traverseWithKey changeVol layerUIMap

  -- apply pan
  let changePan idx layerUI
        = lift $ applyLayerPanChange jamSt idx (layerUI^.layerUIPan)

  _ <- IM.traverseWithKey changePan layerUIMap

  pure (persistedTempo persistedBeat, layerUIMap)
  where
    getPersistedBeat = decode <$> BS.readFile ( savedFilePath <> T.unpack fileName )

deleteSavedBeat :: BeatFileName -> IO ()
deleteSavedBeat (BeatFileName fileName) =
  removeFile $ savedFilePath <> T.unpack fileName
