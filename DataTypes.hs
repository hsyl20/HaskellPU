module DataTypes where

import Structures
import Event
import Task

import Control.Monad
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Alloc
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

floatMatrixRegister :: Ptr () -> CUInt -> CUInt -> CUInt -> IO Handle
floatMatrixRegister ptr nx ny ld = alloca $ \handle -> do
  matrixRegister handle 0 (fromIntegral (ptrToWordPtr ptr)) ld nx ny 4
  peek handle

floatMatrixRegisterInvalid :: Ptr () -> CUInt -> CUInt -> CUInt -> IO FloatMatrix
floatMatrixRegisterInvalid ptr nx ny ld = do
	handle <- floatMatrixRegister ptr nx ny ld
	return FloatMatrix {
		floatMatrixHandle = handle,
		floatMatrixEvent = dummyEvent,
		nx = nx,
		ny = ny,
		ld = ld,
		elemSize = 4
	}

floatMatrixComputeTask :: CUInt -> CUInt -> CUInt -> (Handle -> Task) -> [Event] -> FloatMatrix
floatMatrixComputeTask nx ny ld f deps = unsafePerformIO $ do
  cHandle <- floatMatrixRegister nullPtr nx ny ld
  task <- return $ f cHandle
  fmap (fmap (taskDependsOn task)) (return deps)
  taskSubmit task
  return FloatMatrix {
    floatMatrixHandle = cHandle,
    floatMatrixEvent = taskEvent task,
    nx = nx,
    ny = ny,
    ld = ld,
    elemSize = 4
  }

instance Show FloatMatrix where
  show a = "Matrix[Float](nx = "++ show (nx a) ++
                       "; ny = "++ show (ny a) ++
                       "; ld = "++ show (ld a) ++
                       "; elemsize = "++ show (elemSize a) ++
                       "; handle = "++ show (handle a) ++")"

split :: Int -> Int -> FloatMatrix -> [[FloatMatrix]]
split i j a = undefined

unsplit :: Int -> Int -> [[FloatMatrix]] -> FloatMatrix
unsplit i j a = undefined

crossWith :: (a->b->c) -> [a] -> [b] -> [[c]]
crossWith f a b = map g a
	where
		g x = map (f x) b

cross :: [a] -> [b] -> [[(a,b)]]
cross a b = crossWith (,) a b

rows :: [[a]] -> [[a]]
rows = id

columns :: [[a]] -> [[a]]
columns = transpose

row :: Int -> [[a]] -> [a]
row i m = m !! i

column :: Int -> [[a]] -> [a]
column i m = (columns m) !! i

dropRows :: Int -> [[a]] -> [[a]]
dropRows i m = drop i m

dropColumns :: Int -> [[a]] -> [[a]]
dropColumns i m = transpose $ drop i $ transpose m

fromRows :: [[a]] -> [[a]]
fromRows = id

fromColumns :: [[a]] -> [[a]]
fromColumns = transpose
