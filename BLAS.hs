module BLAS where

import DataTypes
import Task
import Structures

import System.IO.Unsafe

foreign import ccall unsafe "sgemm_task_create" sgemmTaskCreate :: Handle -> Handle -> Handle -> Task

sgemm :: FloatMatrix -> FloatMatrix -> FloatMatrix
sgemm a b = floatMatrixComputeTask h w w f deps
  where
    h = fromIntegral $ nx b
    w = fromIntegral $ ny a
    deps = [event a, event b]
    f h = sgemmTaskCreate (handle a) (handle b) h
