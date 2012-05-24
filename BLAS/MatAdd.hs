module BLAS.MatAdd where

import StarPU.Data
import StarPU.Data.Matrix
import StarPU.Task
import StarPU.Structures

foreign import ccall unsafe "matadd_task_create" mataddTaskCreate :: Handle -> Handle -> Handle -> Task

matadd :: Matrix Float -> Matrix Float -> Matrix Float
matadd a b = floatMatrixComputeTask w h w f deps
  where
    w = width a
    h = height a
    deps = [event a, event b]
    f h = mataddTaskCreate (handle a) (handle b) h

