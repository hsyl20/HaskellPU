module ViperVM.View where

import ViperVM.Buffer

-- |
-- A view describes a set of memory cells in a buffer organized in a given pattern
--
data View =
    View1D { buffer :: Buffer, offset :: Integer, size     :: Integer }
  | View2D { buffer :: Buffer, offset :: Integer, elemSize :: Integer, count :: Integer, padding :: Int}
