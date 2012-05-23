module BLAS.STRSM where

import StarPU.Data.Matrix
import StarPU.Task
import StarPU.Structures

foreign import ccall unsafe "strsm_task_create" strsmTaskCreate :: Handle -> Handle -> Handle -> Task

strsm :: Matrix Float -> Matrix Float -> Matrix Float
strsm a b = floatMatrixComputeTask w h w f deps
  where
    w = width b
    h = height a
    deps = [event a, event b]
    f h = strsmTaskCreate (handle a) (handle b) h
