module StarPU.Data.FloatMatrix where

import Data.Word
import StarPU.Data
import StarPU.Data.Matrix
import StarPU.Task
import StarPU.Structures
import StarPU.Platform
import HighDataTypes

foreign import ccall unsafe "floatmatrix_add_task_create" floatMatrixAddTaskCreate :: Handle -> Handle -> Handle -> Task
foreign import ccall unsafe "floatmatrix_sub_task_create" floatMatrixSubTaskCreate :: Handle -> Handle -> Handle -> Task
foreign import ccall unsafe "floatmatrix_mul_task_create" floatMatrixMulTaskCreate :: Handle -> Handle -> Handle -> Task
foreign import ccall unsafe "floatmatrix_set_task_create" floatMatrixSetTaskCreate :: Float -> Handle -> Task
foreign import ccall unsafe "floatmatrix_tranpose_task_create" floatMatrixTransposeTaskCreate :: Handle -> Handle -> Task

floatMatrixOp :: (Handle -> Handle -> Handle -> Task) -> Matrix Float -> Matrix Float -> Word -> Word -> Matrix Float 
floatMatrixOp g a b w h = floatMatrixComputeTask w h w f deps
  where
    deps = [event a, event b]
    f = g (handle a) (handle b)

{-# RULES
"reduce_plus" forall x y z .  floatMatrixAdd (floatMatrixAdd x y) z = reduce floatMatrixAdd (HighVector [x,y,z])
"reduce_plus_add" forall xs y .  floatMatrixAdd (reduce floatMatrixAdd (HighVector xs)) y = reduce floatMatrixAdd (HighVector (xs ++ [y]))

"reduce_sub" forall x y z .  floatMatrixSub (floatMatrixSub x y) z = reduce floatMatrixSub (HighVector [x,y,z])
"reduce_sub_add" forall xs y .  floatMatrixSub (reduce floatMatrixSub (HighVector xs)) y = reduce floatMatrixSub (HighVector (xs ++ [y]))

"reduce_mul" forall x y z .  floatMatrixMul (floatMatrixMul x y) z = reduce floatMatrixMul (HighVector [x,y,z])
"reduce_mul_add" forall xs y .  floatMatrixMul (reduce floatMatrixMul (HighVector xs)) y = reduce floatMatrixMul (HighVector (xs ++ [y]))
  #-}

{-# NOINLINE floatMatrixAdd #-}
{-# NOINLINE floatMatrixSub #-}
{-# NOINLINE floatMatrixMul #-}

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

floatMatrixSet :: Word -> Word -> Float -> Matrix Float
floatMatrixSet w h v = floatMatrixComputeTask w h w (floatMatrixSetTaskCreate v) []

floatMatrixTranspose :: Matrix Float -> Matrix Float
floatMatrixTranspose m = floatMatrixComputeTask h w h f deps
  where
    deps = [event m]
    h = height m
    w = width m
    f = floatMatrixTransposeTaskCreate (handle m)
