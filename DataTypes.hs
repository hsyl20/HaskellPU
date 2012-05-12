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

floatMatrixRegister :: Ptr () -> Word -> Word -> Word -> IO Handle
floatMatrixRegister ptr nx ny ld = alloca $ \handle -> do
  matrixRegister handle 0 (fromIntegral (ptrToWordPtr ptr)) (fromIntegral ld) (fromIntegral nx) (fromIntegral ny) 4
  peek handle

floatMatrixInit :: (Word -> Word -> Float) -> Word -> Word -> FloatMatrix
floatMatrixInit f width height = unsafePerformIO $ do
  ptr <- starpuMalloc $ fromIntegral (width*height*4) 
  pokeArray (castPtr ptr) cells
  handle <- floatMatrixRegister ptr width height width
  return $ FloatMatrix handle dummyEvent width height width 4
  where
    rows = range (0,height-1)
    cols = range (0,width-1)
    cells = concat $ map (\row -> map (\col -> f row col) cols) rows

floatMatrixRegisterInvalid :: Ptr () -> Word -> Word -> Word -> IO FloatMatrix
floatMatrixRegisterInvalid ptr nx ny ld = do
	handle <- floatMatrixRegister ptr nx ny ld
	return FloatMatrix {
		floatMatrixHandle = handle,
		floatMatrixEvent = dummyEvent,
		nx = fromIntegral nx,
		ny = fromIntegral ny,
		ld = fromIntegral ld,
		elemSize = 4
	}

floatMatrixComputeTask :: Word -> Word -> Word -> (Handle -> Task) -> [Event] -> FloatMatrix
floatMatrixComputeTask nx ny ld f deps = unsafePerformIO $ do
  cHandle <- floatMatrixRegister nullPtr nx ny ld
  task <- return $ f cHandle
  fmap (fmap (taskDependsOn task)) (return deps)
  taskSubmit task
  return FloatMatrix {
    floatMatrixHandle = cHandle,
    floatMatrixEvent = taskEvent task,
    nx = fromIntegral nx,
    ny = fromIntegral ny,
    ld = fromIntegral ld,
    elemSize = 4
  }

instance Show FloatMatrix where
  show a = "Matrix[Float](nx = "++ show (nx a) ++
                       "; ny = "++ show (ny a) ++
                       "; ld = "++ show (ld a) ++
                       "; elemsize = "++ show (elemSize a) ++
                       "; handle = "++ show (handle a) ++")"

split :: Int -> Int -> FloatMatrix -> HighMatrix FloatMatrix
split i j a = undefined

unsplit :: Int -> Int -> HighMatrix FloatMatrix -> FloatMatrix
unsplit i j a = undefined

