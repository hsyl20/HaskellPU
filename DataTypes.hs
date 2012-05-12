module DataTypes where

import HighDataTypes
import Structures
import Event
import Task

import Data.Word
import Data.Ix
import Control.Monad
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.IO.Unsafe
import Data.List

foreign import ccall "starpu_data.h starpu_data_unregister" dataUnregister :: Handle -> IO ()
foreign import ccall "starpu_data.h starpu_data_unregister_no_coherency" dataUnregisterInvalid :: Handle -> IO ()
foreign import ccall "starpu_data.h starpu_data_invalidate" dataInvalidate :: Handle -> IO ()
foreign import ccall "starpu_data.h starpu_data_release" dataRelease :: Handle -> IO ()

unregister :: Data a => a -> IO ()
unregister a = dataUnregister (handle a)

unregisterInvalid :: Data a => a -> IO ()
unregisterInvalid a = dataUnregisterInvalid (handle a)

invalidate :: Data a => a -> IO ()
invalidate a = dataInvalidate (handle a)

release :: Data a => a -> IO ()
release a = dataRelease (handle a)

foreign import ccall unsafe "starpu_data_interfaces.h starpu_matrix_data_register" matrixRegister :: Ptr Handle -> CUInt -> CUIntPtr -> CUInt -> CUInt -> CUInt -> CSize -> IO ()

foreign import ccall unsafe "starpu_data_interfaces.h starpu_variable_data_register" variableRegister :: Ptr Handle -> CUInt -> CUIntPtr -> CSize -> IO ()

foreign import ccall unsafe "starpu_malloc_ex" starpuMalloc :: CSize -> IO (Ptr ())

data FloatMatrix = FloatMatrix {
	floatMatrixHandle :: Handle,
  floatMatrixEvent :: Event,
	nx :: Word,
	ny :: Word,
	ld :: Word,
	elemSize :: Word
}

instance Data FloatMatrix where
  handle = floatMatrixHandle
  event = floatMatrixEvent

-- |Register a StarPU matrix a Float stored at the given address
floatMatrixRegister :: Ptr () -> Word -> Word -> Word -> IO Handle
floatMatrixRegister ptr width height ld = alloca $ \handle -> do
  matrixRegister handle 0 nptr nld nx ny 4
  peek handle
  where
    nptr = fromIntegral $ ptrToWordPtr ptr
    nld = fromIntegral ld
    nx = fromIntegral width
    ny = fromIntegral height

-- |Initialize a new matrix of Float using the given function
floatMatrixInit :: (Word -> Word -> Float) -> Word -> Word -> FloatMatrix
floatMatrixInit f width height = unsafePerformIO $ do
  ptr <- starpuMalloc rawSize
  pokeArray (castPtr ptr) cells
  handle <- floatMatrixRegister ptr width height width
  return $ FloatMatrix handle dummyEvent width height width 4
  where
    rawSize = fromIntegral (width*height*4)
    rows = range (0,height-1)
    cols = range (0,width-1)
    cells = concat $ map (\row -> map (\col -> f row col) cols) rows

floatMatrixComputeTask :: Word -> Word -> Word -> (Handle -> Task) -> [Event] -> FloatMatrix
floatMatrixComputeTask nx ny ld f deps = unsafePerformIO $ do
  handle <- floatMatrixRegister nullPtr nx ny ld
  task <- return $ f handle
  fmap (fmap (taskDependsOn task)) (return deps)
  taskSubmit task
  return $ FloatMatrix handle (taskEvent task) nx ny ld 4

instance Show FloatMatrix where
  show (FloatMatrix handle event nx ny ld elemSize)  =
    "Matrix[Float](nx = "++ show nx ++
    "; ny = "++ show ny ++
    "; ld = "++ show ld ++
    "; elemsize = "++ show elemSize ++
    "; handle = "++ show handle ++")"

split :: Int -> Int -> FloatMatrix -> HighMatrix FloatMatrix
split i j a = undefined

unsplit :: Int -> Int -> HighMatrix FloatMatrix -> FloatMatrix
unsplit i j a = undefined

