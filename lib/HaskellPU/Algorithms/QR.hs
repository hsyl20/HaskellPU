module HaskellPU.Algorithms.QR where

import qualified Prelude
import Prelude (($), (-), (==), Int, Float, undefined)
import Data.Word
import HaskellPU.HighDataTypes
import HaskellPU.Data.Matrix
import Data.Functor

unsplit :: Word -> Word -> HighMatrix (Matrix Float) -> Matrix Float
unsplit _ _ _ = undefined

qr_lu f11 f1k fk1 fkk i a = unsplit i i bs
  where
    as = split i i a
    (a11,ak1,a1k,akk) = triangularSplit as
    b11 = f11 a11
    bk1 = fmap (fk1 b11) ak1
    b1k = fmap (f1k b11) a1k
    tmpkk = zipWith fkk akk (cross bk1 b1k)
    bkk = if (hwidth tmpkk == 0) then tmpkk else split (i-1) (i-1) $ qr_lu f11 f1k fk1 fkk i $ unsplit (i-1) (i-1) tmpkk
    bs = fromTriangularSplit b11 bk1 b1k bkk

triangularSplit :: HighMatrix a -> (a, HighVector a, HighVector a, HighMatrix a)
triangularSplit m = (m11,mk1,m1k,mkk)
  where
    m11 = head (column 1 m)
    mk1 = tail (column 1 m)
    m1k = tail (row 1 m)
    mkk = dropRows 1 (dropColumns 1 m)

fromTriangularSplit :: a -> HighVector a -> HighVector a -> HighMatrix a -> HighMatrix a
fromTriangularSplit m11 mk1 m1k mkk = fromColumns $ cons left right
  where
    left = cons m11 mk1
    right = columns $ fromRows $ cons m1k (rows mkk)
