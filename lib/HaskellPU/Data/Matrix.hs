{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}

module HaskellPU.Data.Matrix where

import HaskellPU.Event
import HaskellPU.Data

import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C

{-------------------
 - Foreign imports 
 -------------------}

foreign import ccall unsafe "starpu_matrix_data_register" matrixRegister :: Ptr UnsafeHandle -> Int -> Ptr () -> Word -> Word -> Word -> CSize -> IO ()
foreign import ccall unsafe "starpu_variable_data_register" variableRegister :: Ptr UnsafeHandle -> Word -> Ptr () -> CSize -> IO ()
foreign import ccall "starpu_matrix_get_local_ptr" matrixLocalPtr :: UnsafeHandle -> IO (Ptr ())

{-------------------
 - Data and instances
 -------------------}

data Matrix a = Matrix {
  matrixHandle :: Handle,
  matrixEvent :: Event,
  width :: Word,
  height :: Word,
  ld :: Word,
  elemSize :: Word
}

instance Data (Matrix a) where
  handle = matrixHandle
  event = matrixEvent

instance Show (Matrix a) where
  show (Matrix { .. })  =
    "Matrix(width = "++ show width ++
    "; height = "++ show height ++
    "; ld = "++ show ld ++
    "; elemsize = "++ show elemSize ++
    "; handle = "++ show matrixHandle ++")"

instance Computable (Matrix a) where
  compute a = withForeignPtr (handle a) $ dataForceCompute
  wait a = eventWait (event a)
