module HaskellPU.Algorithms.Cholesky where

import Prelude hiding (head,tail,zipWith)
import Data.Word

import HaskellPU.HighDataTypes
import HaskellPU.Data.Matrix
import HaskellPU.Data.FloatMatrix
import HaskellPU.Data.TriangularMatrix

cholesky :: Word -> Matrix Float -> HighMatrix (Matrix Float)
cholesky bsize m = choleskyTiled $ split n n m
  where
    n = div (width m) bsize

choleskyTiled :: HighMatrix (Matrix Float) -> HighMatrix (Matrix Float)
choleskyTiled ms = if hheight ms == 0 then ms else fromTriangularSplit l11 l12 l12 l22
  where
    l11 = floatMatrixPotrf m11
    l12 = fmap (strsm 0 (LowerTriangularMatrix l11 False)) m12
    l22 = choleskyTiled $ zipWith (-) m22 (crossWith (*) l12 l12)
    (m11,m12,_,m22) = triangularSplit ms
    
triangularSplit :: HighMatrix a -> (a, HighVector a, HighVector a, HighMatrix a)
triangularSplit m = (m11,mk1,m1k,mkk)
  where
    m11 = head (column 0 m)
    mk1 = tail (column 0 m)
    m1k = tail (row 0 m)
    mkk = dropRows 1 (dropColumns 1 m)

fromTriangularSplit :: a -> HighVector a -> HighVector a -> HighMatrix a -> HighMatrix a
fromTriangularSplit m11 mk1 m1k mkk = fromColumns $ cons left right
  where
    left = cons m11 mk1
    right = columns $ fromRows $ cons m1k (rows mkk)
