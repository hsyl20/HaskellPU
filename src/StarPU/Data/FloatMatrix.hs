module StarPU.Data.FloatMatrix where

import Data.Word
import StarPU.Data
import StarPU.Data.Matrix
import StarPU.Task
import StarPU.Structures
import StarPU.Platform
import HighDataTypes

{-------------------
 - Foreign imports 
 -------------------}

foreign import ccall unsafe "floatmatrix_add_task_create" floatMatrixAddTaskCreate :: Handle -> Handle -> Handle -> Task
foreign import ccall unsafe "floatmatrix_sub_task_create" floatMatrixSubTaskCreate :: Handle -> Handle -> Handle -> Task
foreign import ccall unsafe "floatmatrix_mul_task_create" floatMatrixMulTaskCreate :: Handle -> Handle -> Handle -> Task
foreign import ccall unsafe "floatmatrix_set_task_create" floatMatrixSetTaskCreate :: Float -> Handle -> Task
foreign import ccall unsafe "floatmatrix_transpose_task_create" floatMatrixTransposeTaskCreate :: Handle -> Handle -> Task
foreign import ccall unsafe "floatmatrix_scale_task_create" floatMatrixScaleTaskCreate :: Float -> Handle -> Handle -> Task
foreign import ccall unsafe "floatmatrix_spotrf_task_create" floatMatrixSpotrfTaskCreate :: Handle -> Handle -> Task

{-------------------
 - Operations
 -------------------}

floatMatrixBinOp :: (Handle -> Handle -> Handle -> Task) -> Matrix Float -> Matrix Float -> Word -> Word -> Matrix Float 
floatMatrixBinOp g a b w h = floatMatrixComputeTask w h w f deps
  where
    deps = [event a, event b]
    f = g (handle a) (handle b)

floatMatrixUnaryOp :: (Handle -> Handle -> Task) -> Matrix Float -> Word -> Word -> Matrix Float
floatMatrixUnaryOp g m w h = floatMatrixComputeTask w h w f deps
  where
    deps = [event m]
    f = g (handle m)

floatMatrixAdd :: Matrix Float -> Matrix Float -> Matrix Float
floatMatrixAdd a b = floatMatrixBinOp floatMatrixAddTaskCreate a b (width a) (height a)

floatMatrixSub :: Matrix Float -> Matrix Float -> Matrix Float
floatMatrixSub a b = floatMatrixBinOp floatMatrixSubTaskCreate a b (width a) (height a)

floatMatrixMul :: Matrix Float -> Matrix Float -> Matrix Float
floatMatrixMul a b = floatMatrixBinOp floatMatrixMulTaskCreate a b (width b) (height a)

floatMatrixSet :: Word -> Word -> Float -> Matrix Float
floatMatrixSet w h v = floatMatrixComputeTask w h w (floatMatrixSetTaskCreate v) []

floatMatrixTranspose :: Matrix Float -> Matrix Float
floatMatrixTranspose m = floatMatrixUnaryOp floatMatrixTransposeTaskCreate m (height m) (width m)

floatMatrixScale :: Float -> Matrix Float -> Matrix Float
floatMatrixScale v m = floatMatrixUnaryOp (floatMatrixScaleTaskCreate v) m (width m) (height m)

floatMatrixPotrf :: Matrix Float -> Matrix Float
floatMatrixPotrf m = floatMatrixUnaryOp floatMatrixSpotrfTaskCreate m (width m) (height m)

{-------------------
 - Rewrite Rules
 -------------------}

{-# NOINLINE floatMatrixAdd #-}
{-# NOINLINE floatMatrixSub #-}
{-# NOINLINE floatMatrixMul #-}
{-# NOINLINE floatMatrixTranspose #-}
{-# NOINLINE floatMatrixScale #-}

{-# RULES
"reduce_plus" forall x y z .  floatMatrixAdd (floatMatrixAdd x y) z = reduce floatMatrixAdd (HighVector [x,y,z])
"reduce_plus_add" forall xs y .  floatMatrixAdd (reduce floatMatrixAdd (HighVector xs)) y = reduce floatMatrixAdd (HighVector (xs ++ [y]))

"reduce_sub" forall x y z .  floatMatrixSub (floatMatrixSub x y) z = reduce floatMatrixSub (HighVector [x,y,z])
"reduce_sub_add" forall xs y .  floatMatrixSub (reduce floatMatrixSub (HighVector xs)) y = reduce floatMatrixSub (HighVector (xs ++ [y]))

"reduce_mul" forall x y z .  floatMatrixMul (floatMatrixMul x y) z = reduce floatMatrixMul (HighVector [x,y,z])
"reduce_mul_add" forall xs y .  floatMatrixMul (reduce floatMatrixMul (HighVector xs)) y = reduce floatMatrixMul (HighVector (xs ++ [y]))

"transpose_transpose" forall m . floatMatrixTranspose (floatMatrixTranspose m) = m

"scale_scale" forall f1 f2 m . floatMatrixScale f1 (floatMatrixScale f2 m) = floatMatrixScale (f1 * f2) m
  #-}

{-------------------
 - Instances
 -------------------}

instance Num (Matrix Float) where
  (*) = floatMatrixMul
  (+) = floatMatrixAdd
  (-) = floatMatrixSub
  abs = undefined
  signum = undefined
  fromInteger = undefined

instance Eq (Matrix Float) where
  (==) = undefined
