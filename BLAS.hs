module BLAS where

import DataTypes
import Task
import Structures

import System.IO.Unsafe

foreign import ccall unsafe "sgemm_task_create" sgemmTaskCreate :: Handle -> Handle -> Handle -> Task

sgemm :: FloatMatrix -> FloatMatrix -> FloatMatrix
sgemm a b = floatMatrixComputeTask (nx b) (ny a) (ny a) f deps
  where
    deps = [event a, event b]
    f h = sgemmTaskCreate (handle a) (handle b) h
