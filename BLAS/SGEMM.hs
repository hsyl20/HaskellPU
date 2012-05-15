module BLAS.SGEMM where

import StarPU.Data.Matrix
import StarPU.Task
import StarPU.Structures

foreign import ccall unsafe "sgemm_task_create" sgemmTaskCreate :: Handle -> Handle -> Handle -> Task

sgemm :: Matrix Float -> Matrix Float -> Matrix Float
sgemm a b = floatMatrixComputeTask h w w f deps
  where
    h = nx b
    w = ny a
    deps = [event a, event b]
    f h = sgemmTaskCreate (handle a) (handle b) h
