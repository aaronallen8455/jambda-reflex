{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
module Jambda.Types.Cell
  ( Cell (..)
  , Cell'
  , cellValue
  , cellSource
  ) where

import            Control.Lens

import            Jambda.Types.SoundSource
import            Jambda.Types.Newtypes (CellValue)

data Cell a =
  Cell
    { _cellValue  :: !a                    -- ^ The rhythmic value of the cell
    , _cellSource :: !(Maybe SoundSource)  -- ^ Overrides the sound of the layer
    }
    deriving (Functor)

type Cell' = Cell CellValue

makeLenses ''Cell
