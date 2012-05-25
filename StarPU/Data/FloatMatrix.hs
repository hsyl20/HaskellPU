module StarPU.Data.FloatMatrix where

import Data.Word
import StarPU.Data
import StarPU.Data.Matrix
import StarPU.Task
import StarPU.Structures

foreign import ccall unsafe "floatmatrix_add_task_create" floatMatrixAddTaskCreate :: Handle -> Handle -> Handle -> Task
foreign import ccall unsafe "floatmatrix_sub_task_create" floatMatrixSubTaskCreate :: Handle -> Handle -> Handle -> Task
foreign import ccall unsafe "floatmatrix_mul_task_create" floatMatrixMulTaskCreate :: Handle -> Handle -> Handle -> Task

floatMatrixOp :: (Handle -> Handle -> Handle -> Task) -> Matrix Float -> Matrix Float -> Word -> Word -> Matrix Float 
floatMatrixOp g a b w h = floatMatrixComputeTask w h w f deps
  where
    deps = [event a, event b]
    f = g (handle a) (handle b)

floatMatrixAdd :: Matrix Float -> Matrix Float -> Matrix Float
floatMatrixAdd a b = floatMatrixOp floatMatrixAddTaskCreate a b (width a) (height a)

floatMatrixSub :: Matrix Float -> Matrix Float -> Matrix Float
floatMatrixSub a b = floatMatrixOp floatMatrixSubTaskCreate a b (width a) (height a)

floatMatrixMul :: Matrix Float -> Matrix Float -> Matrix Float
floatMatrixMul a b = floatMatrixOp floatMatrixMulTaskCreate a b (width b) (height a)

instance Num (Matrix Float) where
  (*) = floatMatrixMul
  (+) = floatMatrixAdd
  (-) = floatMatrixSub
  abs = undefined
  signum = undefined
  fromInteger = undefined

instance Eq (Matrix Float) where
  (==) = undefined
