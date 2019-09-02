module Jambda.Data.Parsers
  ( parseBeat
  , parseBpm
  , parseVol
  , parsePitch
  , parseOffset
  ) where

import           Control.Lens hiding (simple)
import           Control.Monad (guard, join)
import           Control.Monad.Trans (lift)
import           Control.Monad.State (execStateT)
import           Data.Foldable (for_)
import           Data.Functor (($>))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.IntMap as M
import           Data.Semigroup (sconcat)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Void (Void)
import           GHC.Exts (IsList(..))
import           GHC.Float (double2Float)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Jambda.Types

type Parser = Parsec Void T.Text

parseBeat :: Int
          -> M.IntMap T.Text
          -> V.Vector Wav
          -> T.Text
          -> Maybe (NonEmpty Cell')
parseBeat idx refMap wavs = join . parseMaybe ( beatP idx refMap wavs <* eof )

parseOffset :: T.Text -> Maybe CellValue
parseOffset = parseMaybe ( cellValueP ( >= 0 ) )

parseBpm :: T.Text -> Maybe BPM
parseBpm = parseMaybe bpmP

parseVol :: T.Text -> Maybe Vol
parseVol = parseMaybe volP

parsePitch :: T.Text -> Maybe Pitch
parsePitch = parseMaybe pitchP

bpmP :: Parser BPM
bpmP = do
  v <- doubleP
  guard $ v > 0
  pure $ BPM v

volP :: Parser Vol
volP = do
  v <- double2Float <$> doubleP
  guard $ v >= 0 && v <= 10
  pure $ Vol v

doubleP :: Parser Double
doubleP = read <$> ( try mixedP <|> fmap ('0':) fracP )
  where
    numP   = some digitChar
    fracP  = (:) <$> char '.' <*> numP
    mixedP = do
      a <- numP
      b <- fmap (maybe "" id) $ optional fracP
      pure $ a ++ b

data Operator
  = Add
  | Sub
  | Mul
  | Div

operatorP :: Parser Operator
operatorP = char '+' $> Add
        <|> char '-' $> Sub
        <|> char '*' $> Mul
        <|> char '/' $> Div

expressionP :: Parser Double
expressionP = fmap ( uncurry (+) )
            . ( `execStateT` ( 0, 0 ) ) $ do

  _2 <~ lift ( doubleP <* space )

  terms <- lift . many $ (,) <$> ( operatorP <* space )
                             <*> ( doubleP   <* space )

  let add n = do p  <- use _2
                 _1 += p
                 _2 .= n
      mult n = _2 *= n

  for_ terms $ \( operator, num ) ->
    case operator of
      Add -> add num
      Sub -> add $ negate num
      Mul -> mult num
      Div -> if num == 0
                then failure ( Just . Tokens $ fromList "divide by 0" ) mempty
                else mult ( 1 / num )

cellValueP :: (Double -> Bool) -> Parser CellValue
cellValueP pred' = do
  v <- expressionP
  guard $ pred' v
  pure $ CellValue v

soundSourceP :: V.Vector Wav -> Parser SoundSource
soundSourceP wavs = char '@' *> parser <* space
  where
    parser
        = ( SSPitch <$> pitchP )
      <|> ( SSWav   <$> wavP   )
    wavP = do
      i <- pred <$> intP
      case wavs ^? ix i of
        Just wav -> pure wav
        Nothing  -> fail "invalid wav index"

cellP :: V.Vector Wav -> (Double -> Bool) -> Parser Cell'
cellP wavs pred' = do
  v <- cellValueP pred'
  p <- optional $ soundSourceP wavs
  pure $ Cell v p

refP :: Int -> M.IntMap T.Text -> V.Vector Wav -> Parser (Maybe (NonEmpty Cell'))
refP idx refMap wavs = do
  refIdx <- char '$' *> ( ( pred <$> intP ) <|> ( idx <$ char' 's' ) )
  guard $ refIdx >= 0
  case M.lookup refIdx refMap of
    Nothing -> pure Nothing
    Just refStr ->
      maybe ( fail "reference failed to parse" )
            ( pure . Just )
            ( parseBeat idx ( M.delete refIdx refMap ) wavs refStr )

repableP :: Int -> M.IntMap T.Text -> V.Vector Wav -> Parser (Maybe (NonEmpty Cell'))
repableP idx refMap wavs = try ( refP idx refMap wavs )
                       <|> ( Just . pure <$> cellP wavs ( > 0 ) )

repCellP :: Int -> M.IntMap T.Text -> V.Vector Wav -> Parser (Maybe (NonEmpty Cell'))
repCellP idx refMap wavs = do
  mbCells <- repableP idx refMap wavs <* space :: Parser (Maybe (NonEmpty Cell'))
  case mbCells of
    Nothing -> pure Nothing
    Just cells -> do
      ( reps, ltm ) <- fmap ( maybe ( 1, 0 ) id ) . optional $ do
        reps <- between ( char '(' <* space )
                        ( char ')' <* space )
                        intP
        ltm <- maybe 0 id <$> optional expressionP <* space
        pure ( reps, CellValue ltm )
      let ( lastCell :| rest ) = NonEmpty.reverse cells
      guard $ reps > 0 && lastCell^.cellValue + ltm > 0
      pure . Just . sconcat . NonEmpty.fromList
           . reverse $ ( NonEmpty.reverse $ fmap ( + ltm ) lastCell :| rest )
                     : replicate ( reps - 1 ) cells

blockRepP :: Int -> M.IntMap T.Text -> V.Vector Wav -> Parser (Maybe (NonEmpty Cell'))
blockRepP idx refMap wavs = do
    mbInner <- between ( char '[' <* space )
                       ( char ']' <* space )
                       ( beatP idx refMap wavs )
    (reps, ltm) <- tagP <* space

    case mbInner of
      Nothing -> pure Nothing
      Just inner -> do
        let ( lastCell :| rest ) = NonEmpty.reverse inner
        guard $ reps > 0 && lastCell^.cellValue + ltm > 0
        pure . Just . sconcat . NonEmpty.reverse
              $ ( NonEmpty.reverse $ fmap ( + ltm ) lastCell :| rest )
             :| ( replicate ( reps - 1 ) inner )
  where
    tagP = try simple <|> complex
    simple = (,) <$> intP <*> pure ( CellValue 0 )
    complex = do
      reps <- between ( char '(' <* space )
                      ( char ')' <* space )
                      intP
      ltm <- expressionP
      pure (reps, CellValue ltm)

blockMultP :: Int -> M.IntMap T.Text -> V.Vector Wav -> Parser (Maybe (NonEmpty Cell'))
blockMultP idx refMap wavs = do
  mbInner <- between ( char '{' <* space )
                     ( char '}' <* space )
                     ( beatP idx refMap wavs )
  factor <- CellValue <$> expressionP
  guard $ factor > 0
  pure $ ( fmap . fmap . fmap ) ( * factor ) mbInner

-- The return value is a Maybe because if a layer reference cannot be resolved
-- due to it occuring recursively, we want to delete that cell without the
-- parser failing. The Semigroup instance of Maybe NonEmpty facilitates this;
-- the Nothing case only occurs in the event of a rejected reference key
beatP :: Int -> M.IntMap T.Text -> V.Vector Wav -> Parser (Maybe (NonEmpty Cell'))
beatP idx refMap wavs = fmap sconcat . nonEmptyGuard
      $ space *> (   try ( repCellP idx refMap wavs )
                 <|> try ( blockRepP idx refMap wavs )
                 <|> blockMultP idx refMap wavs
                 ) `sepBy1` ( char ',' <* space )

nonEmptyGuard :: Parser [a] -> Parser (NonEmpty a)
nonEmptyGuard p = do
  xs <- p
  guard . not $ null xs
  pure $ NonEmpty.fromList xs

pitchP :: Parser Pitch
pitchP = try ( Pitch <$> ( ANat  <$   string' "A"                     ) <*> octaveP )
     <|> try ( Pitch <$> ( BFlat <$ ( string' "Bb" <|> string' "A#" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( BNat  <$ ( string' "B"  <|> string' "Cb" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( CNat  <$ ( string' "C"  <|> string' "B#" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( DFlat <$ ( string' "C#" <|> string' "Db" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( DNat  <$   string' "D"                     ) <*> octaveP )
     <|> try ( Pitch <$> ( EFlat <$ ( string' "Eb" <|> string' "D#" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( ENat  <$ ( string' "E"  <|> string' "Fb" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( FNat  <$ ( string' "F"  <|> string' "E#" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( GFlat <$ ( string' "F#" <|> string' "Gb" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( GNat  <$   string' "G"                     ) <*> octaveP )
     <|>     ( Pitch <$> ( AFlat <$ ( string' "G#" <|> string' "Ab" ) ) <*> octaveP )

octaveP :: Parser Octave
octaveP = do
  n <- intP
  guard $ n > 0 && n <= 12
  pure $ Octave n

intP :: Parser Int
intP = read <$> some digitChar
