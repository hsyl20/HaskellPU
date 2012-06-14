module ViperVM.Data.Matrix where

import ViperVM.AccessMode
import ViperVM.Event
import ViperVM.Data
import ViperVM.Structures
import ViperVM.Task

import Data.Ix
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.Storable
import System.IO.Unsafe
import System.Mem.Weak

{-------------------
 - Foreign imports 
 -------------------}

foreign import ccall unsafe "starpu_matrix_data_register" matrixRegister :: Ptr UnsafeHandle -> Word -> WordPtr -> Word -> Word -> Word -> CSize -> IO ()
foreign import ccall unsafe "starpu_variable_data_register" variableRegister :: Ptr UnsafeHandle -> Word -> WordPtr -> CSize -> IO ()
foreign import ccall "starpu_matrix_get_local_ptr" matrixLocalPtr :: UnsafeHandle -> IO WordPtr

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
  show (Matrix handle event w h ld elemSize)  =
    "Matrix(width = "++ show w ++
    "; height = "++ show h ++
    "; ld = "++ show ld ++
    "; elemsize = "++ show elemSize ++
    "; handle = "++ show handle ++")"

instance Computable (Matrix a) where
  compute a = withForeignPtr (handle a) $ dataForceCompute
  wait a = eventWait (event a)
