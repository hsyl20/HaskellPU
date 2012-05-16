module StarPU.Data.Matrix where

import StarPU.AccessMode
import StarPU.DataTypes
import StarPU.Event
import StarPU.Structures
import StarPU.Task

import Data.Ix
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

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

floatMatrixRegisterInvalid :: Word -> Word -> IO Handle
floatMatrixRegisterInvalid width height = do
  ptr <- starpuMalloc rawSize
  floatMatrixRegister ptr width height width
  where
    rawSize = fromIntegral (width*height*4)


floatMatrixComputeTask :: Word -> Word -> Word -> (Handle -> Task) -> [Event] -> Matrix Float
floatMatrixComputeTask nx ny ld f deps = unsafePerformIO $ do
  --FIXME: StarPU is not able to allocate a matrix with a NULL ptr
  handle <- floatMatrixRegisterInvalid nx ny
  task <- return $ f handle
  mapM (taskDependsOn task) deps
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
    rows = range (0, (ny m) - 1)
    rowOffset row = fromIntegral $ row * (ld m) * (elemSize m)
    rowSize = fromIntegral $ (nx m)

printFloatMatrix :: Matrix Float -> IO String
printFloatMatrix m = do
  ms <- readFloatMatrix m
  return $ unlines $ map show ms

