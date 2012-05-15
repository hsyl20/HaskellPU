module StarPU.DataTypes where

import HighDataTypes
import StarPU.Structures
import StarPU.Event
import StarPU.Task
import StarPU.AccessMode

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

foreign import ccall "starpu_data_acquire" dataAcquire :: Handle -> AccessMode -> IO Int
foreign import ccall "starpu_matrix_get_local_ptr" matrixLocalPtr :: Handle -> IO CUIntPtr

unregister :: Data a => a -> IO ()
unregister a = dataUnregister (handle a)

unregisterInvalid :: Data a => a -> IO ()
unregisterInvalid a = dataUnregisterInvalid (handle a)

invalidate :: Data a => a -> IO ()
invalidate a = dataInvalidate (handle a)

release :: Data a => a -> IO ()
release a = dataRelease (handle a)

acquire :: Data a => AccessMode -> a -> IO Int
acquire mode a = dataAcquire (handle a) mode

foreign import ccall unsafe "starpu_data_interfaces.h starpu_matrix_data_register" matrixRegister :: Ptr Handle -> CUInt -> CUIntPtr -> CUInt -> CUInt -> CUInt -> CSize -> IO ()

foreign import ccall unsafe "starpu_data_interfaces.h starpu_variable_data_register" variableRegister :: Ptr Handle -> CUInt -> CUIntPtr -> CSize -> IO ()

foreign import ccall unsafe "starpu_malloc_ex" starpuMalloc :: CSize -> IO (Ptr ())

data Matrix a = Matrix {
	matrixHandle :: Handle,
  matrixEvent :: Event,
	nx :: Word,
	ny :: Word,
	ld :: Word,
	elemSize :: Word
}

instance Data (Matrix a) where
  handle = matrixHandle
  event = matrixEvent

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
floatMatrixInit :: (Word -> Word -> Float) -> Word -> Word -> Matrix Float
floatMatrixInit f width height = unsafePerformIO $ do
  ptr <- starpuMalloc rawSize
  pokeArray (castPtr ptr) cells
  handle <- floatMatrixRegister ptr width height width
  return $ Matrix handle dummyEvent width height width 4
  where
    rawSize = fromIntegral (width*height*4)
    rows = range (0,height-1)
    cols = range (0,width-1)
    cells = concat $ map (\row -> map (\col -> f row col) cols) rows

floatMatrixComputeTask :: Word -> Word -> Word -> (Handle -> Task) -> [Event] -> Matrix Float
floatMatrixComputeTask nx ny ld f deps = unsafePerformIO $ do
  handle <- floatMatrixRegister nullPtr nx ny ld
  task <- return $ f handle
  fmap (fmap (taskDependsOn task)) (return deps)
  taskSubmit task
  return $ Matrix handle (taskEvent task) nx ny ld 4

instance Show (Matrix a) where
  show (Matrix handle event nx ny ld elemSize)  =
    "Matrix(nx = "++ show nx ++
    "; ny = "++ show ny ++
    "; ld = "++ show ld ++
    "; elemsize = "++ show elemSize ++
    "; handle = "++ show handle ++")"


readFloatMatrix :: Matrix Float -> IO [[Float]]
readFloatMatrix m = do
  acquire readOnly m
  ptr <- matrixLocalPtr (handle m)
  uptr <- return $ wordPtrToPtr $ fromIntegral ptr
  values <- mapM (\row -> (peekArray rowSize (plusPtr uptr (rowOffset row)))) rows
  release m
  return values
  where
    rows = range (0, ny m)
    rowOffset row = fromIntegral $ row * (ld m) * (elemSize m)
    rowSize = fromIntegral $ (nx m)



split :: Int -> Int -> Matrix Float -> HighMatrix (Matrix Float)
split i j a = undefined

unsplit :: Int -> Int -> HighMatrix (Matrix Float) -> Matrix Float
unsplit i j a = undefined

