module StarPU.Data.Matrix where

import StarPU.AccessMode
import StarPU.DataTypes
import StarPU.Event
import StarPU.Data
import StarPU.Structures
import StarPU.Task

import Data.Ix
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import System.IO.Unsafe
import System.Mem.Weak

import HighDataTypes

foreign import ccall unsafe "starpu_matrix_data_register" matrixRegister :: Ptr Handle -> Word -> WordPtr -> Word -> Word -> Word -> CSize -> IO ()
foreign import ccall unsafe "starpu_variable_data_register" variableRegister :: Ptr Handle -> Word -> WordPtr -> CSize -> IO ()
foreign import ccall "starpu_matrix_get_local_ptr" matrixLocalPtr :: Handle -> IO WordPtr
foreign import ccall unsafe "sub_matrix_task_create" subMatrixTaskCreate :: Word -> Word -> Handle -> Handle -> Task
foreign import ccall unsafe "duplicate_matrix_task_create" duplicateMatrixTaskCreate :: Handle -> Handle -> IO Task

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

-- |Register a StarPU matrix a Float stored at the given address
floatMatrixRegister :: Ptr () -> Word -> Word -> Word -> IO Handle
floatMatrixRegister ptr width height ld = alloca $ \handle -> do
  matrixRegister handle 0 nptr nld nx ny 4
  hdl <- peek handle
  addFinalizer hdl $ putStrLn ("TODO: Unregistering (from Haskell) " ++ show hdl)
  return hdl
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
floatMatrixComputeTask w h ld f deps = unsafePerformIO $ do
  --FIXME: StarPU is not able to allocate a matrix with a NULL ptr
  handle <- floatMatrixRegisterInvalid w h
  task <- return $ f handle
  mapM (taskDependsOn task) deps
  taskSubmit task
  return $ Matrix handle (taskEvent task) w h ld 4

floatMatrixDuplicate :: Matrix Float -> IO (Matrix Float)
floatMatrixDuplicate m = do
  hdl <- floatMatrixRegisterInvalid (width m) (height m)
  task <- duplicateMatrixTaskCreate (handle m) hdl
  taskDependsOn task (event m)
  taskSubmit task
  return $ Matrix hdl (taskEvent task) (width m) (height m) (ld m) (elemSize m)

instance Show (Matrix a) where
  show (Matrix handle event w h ld elemSize)  =
    "Matrix(width = "++ show w ++
    "; height = "++ show h ++
    "; ld = "++ show ld ++
    "; elemsize = "++ show elemSize ++
    "; handle = "++ show handle ++")"


readFloatMatrix :: Matrix Float -> IO [[Float]]
readFloatMatrix m = do
  eventWait (event m)
  acquire readOnly m
  ptr <- matrixLocalPtr (handle m)
  uptr <- return $ wordPtrToPtr $ fromIntegral ptr
  values <- mapM (\row -> (peekArray rowSize (plusPtr uptr (rowOffset row)))) rows
  release m
  return values
  where
    rows = range (0, (height m) - 1)
    rowOffset row = fromIntegral $ row * (ld m) * (elemSize m)
    rowSize = fromIntegral $ (width m)

printFloatMatrix :: Matrix Float -> IO ()
printFloatMatrix m = do
  ms <- readFloatMatrix m
  putStrLn $ unlines $ map show ms
  return ()


subMatrix :: Word -> Word -> Word -> Word -> Matrix Float -> Matrix Float
subMatrix x y w h m = floatMatrixComputeTask w h w f deps
  where
    deps = [event m]
    f h = subMatrixTaskCreate x y (handle m) h

split :: Word -> Word -> Matrix Float -> HighMatrix (Matrix Float)
split x y m = HighMatrix $ map (\r -> map (\c -> f c r) cols) rows
  where
    rows = range (0,y-1)
    cols = range (0,x-1)
    w = width m
    h = height m
    wp = div w x
    wr = w - (x*wp)
    hp = div h y
    hr = h - (y*hp)
    f c r = subMatrix (c*wp) (r*hp) myW myH m
      where
        myW = if c /= x-1 then wp else (wp+wr)
        myH = if r /= y-1 then hp else (hp+hr)

traverseHighMatrix :: (a -> IO ()) -> HighMatrix a -> IO ()
traverseHighMatrix g m = f 0 0
  where
    w = hwidth m
    h = hheight m
    HighMatrix r = m
    f x y = do
      if x >= w || y >= h
        then return ()
        else do g (r !! y !! x)
                if x == (w-1)
                  then do f 0 (y+1)
                  else do f (x+1) y

printHighMatrix :: HighMatrix (Matrix Float) -> IO ()
printHighMatrix = traverseHighMatrix printFloatMatrix

waitAndShow m = do
  eventWait (event m)
  putStrLn (show m)
  
showHighMatrix :: HighMatrix (Matrix Float) -> IO ()
showHighMatrix = traverseHighMatrix waitAndShow
