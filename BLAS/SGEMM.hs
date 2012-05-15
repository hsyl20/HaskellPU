module BLAS.SGEMM where

import DataTypes
import StarPU.Task
import StarPU.Structures

foreign import ccall unsafe "sgemm_task_create" sgemmTaskCreate :: Handle -> Handle -> Handle -> Task

sgemm :: FloatMatrix -> FloatMatrix -> FloatMatrix
sgemm a b = floatMatrixComputeTask h w w f deps
  where
    h = nx b
    w = ny a
    deps = [event a, event b]
    f h = sgemmTaskCreate (handle a) (handle b) h
