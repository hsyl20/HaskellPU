module BLAS where

import qualified BLAS.SGEMM
import qualified BLAS.MatAdd

sgemm = BLAS.SGEMM.sgemm
matadd = BLAS.MatAdd.matadd
