module ViperVM.Data.FloatMatrix where

import Data.Ix
import Data.Word
import qualified Data.List

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.Storable
import System.IO.Unsafe
import System.Mem.Weak

import ViperVM.Data
import ViperVM.Data.Matrix
import ViperVM.Task
import ViperVM.Structures
import ViperVM.Platform
import ViperVM.AccessMode
import ViperVM.Event

{-------------------
 - Foreign imports 
 -------------------}

foreign import ccall unsafe "floatmatrix_add_task_create" floatMatrixAddTaskCreate :: UnsafeHandle -> UnsafeHandle -> UnsafeHandle -> IO Task
foreign import ccall unsafe "floatmatrix_sub_task_create" floatMatrixSubTaskCreate :: UnsafeHandle -> UnsafeHandle -> UnsafeHandle -> IO Task
foreign import ccall unsafe "floatmatrix_sgemm_task_create" floatMatrixSgemmTaskCreate :: Float -> Float -> UnsafeHandle -> UnsafeHandle -> UnsafeHandle -> IO Task
foreign import ccall unsafe "floatmatrix_set_task_create" floatMatrixSetTaskCreate :: Float -> UnsafeHandle -> IO Task
foreign import ccall unsafe "floatmatrix_transpose_task_create" floatMatrixTransposeTaskCreate :: UnsafeHandle -> UnsafeHandle -> IO Task
foreign import ccall unsafe "floatmatrix_scale_task_create" floatMatrixScaleTaskCreate :: Float -> UnsafeHandle -> UnsafeHandle -> IO Task
foreign import ccall unsafe "floatmatrix_spotrf_task_create" floatMatrixSpotrfTaskCreate :: UnsafeHandle -> IO Task
foreign import ccall unsafe "floatmatrix_duplicate_task_create" duplicateMatrixTaskCreate :: UnsafeHandle -> UnsafeHandle -> IO Task
foreign import ccall unsafe "floatmatrix_submatrix_task_create" subMatrixTaskCreate :: Word -> Word -> UnsafeHandle -> UnsafeHandle -> IO Task

{-------------------
 - Operations
 -------------------}

floatMatrixAdd :: Matrix Float -> Matrix Float -> Matrix Float
floatMatrixAdd a b = floatMatrixBinOp floatMatrixAddTaskCreate a b (width a) (height a)

floatMatrixSub :: Matrix Float -> Matrix Float -> Matrix Float
floatMatrixSub a b = floatMatrixBinOp floatMatrixSubTaskCreate a b (width a) (height a)

floatMatrixMul :: Matrix Float -> Matrix Float -> Matrix Float
floatMatrixMul a b = floatMatrixBinOp (floatMatrixSgemmTaskCreate 1.0 0.0) a b (width b) (height a)

floatMatrixSet :: Word -> Word -> Float -> Matrix Float
floatMatrixSet w h v = floatMatrixInitOp (floatMatrixSetTaskCreate v) w h

floatMatrixTranspose :: Matrix Float -> Matrix Float
floatMatrixTranspose m = floatMatrixUnaryOp floatMatrixTransposeTaskCreate m (height m) (width m)

floatMatrixScale :: Float -> Matrix Float -> Matrix Float
floatMatrixScale v m = floatMatrixUnaryOp (floatMatrixScaleTaskCreate v) m (width m) (height m)

floatMatrixPotrf :: Matrix Float -> Matrix Float
floatMatrixPotrf m = floatMatrixInplaceUnaryOp floatMatrixSpotrfTaskCreate m

subMatrix :: Word -> Word -> Word -> Word -> Matrix Float -> Matrix Float
subMatrix x y w h m = floatMatrixUnaryOp (subMatrixTaskCreate x y) m w h

floatMatrixDuplicate :: Matrix Float -> IO (Matrix Float)
floatMatrixDuplicate m = do
  hdl <- floatMatrixRegisterInvalid (width m) (height m)
  evt <- withForeignPtr (handle m) $ \hdlM -> withForeignPtr hdl $ \h -> do
    task <- dataDuplicate hdlM h
    taskDependsOn task (event m)
    taskSubmit task
    return $ taskEvent task
  return $ Matrix hdl evt (width m) (height m) (ld m) (elemSize m)



floatMatrixBinOp :: (UnsafeHandle -> UnsafeHandle -> UnsafeHandle -> IO Task) -> Matrix Float -> Matrix Float -> Word -> Word -> Matrix Float 
floatMatrixBinOp g a b w h = floatMatrixComputeTask w h h f deps
  where
    deps = [event a, event b]
    f x = withForeignPtr (handle a) $ \hdlA -> withForeignPtr (handle b) $ \hdlB -> g hdlA hdlB x

floatMatrixUnaryOp :: (UnsafeHandle -> UnsafeHandle -> IO Task) -> Matrix Float -> Word -> Word -> Matrix Float
floatMatrixUnaryOp g m w h = floatMatrixComputeTask w h h f deps
  where
    deps = [event m]
    f x = withForeignPtr (handle m) $ \hdlM -> g hdlM x

floatMatrixInitOp :: (UnsafeHandle -> IO Task) -> Word -> Word -> Matrix Float
floatMatrixInitOp g w h = floatMatrixComputeTask w h h g deps
  where
    deps = []

floatMatrixInplaceUnaryOp :: (UnsafeHandle -> IO Task) -> Matrix Float -> Matrix Float
floatMatrixInplaceUnaryOp f m = floatMatrixInplaceTask f deps m
  where
    deps = [event m]

floatMatrixInplaceTask :: (UnsafeHandle -> IO Task) -> [Event] -> Matrix Float -> Matrix Float
floatMatrixInplaceTask f deps m = unsafePerformIO $ do
  dupMat <- floatMatrixDuplicate m
  task <- withForeignPtr (handle dupMat) $ \hdl -> do
    task <- f hdl
    mapM (taskDependsOn task) deps
    taskSubmit task
    return task
  return $ Matrix (handle dupMat) (taskEvent task) (width dupMat) (height dupMat) (ld dupMat) 4

floatMatrixComputeTask :: Word -> Word -> Word -> (UnsafeHandle -> IO Task) -> [Event] -> Matrix Float
floatMatrixComputeTask w h ld f deps = unsafePerformIO $ do
  handle <- floatMatrixRegisterInvalid w h
  task <- withForeignPtr handle $ \hdl -> do
    task <- f hdl
    mapM (taskDependsOn task) deps
    taskSubmit task
    return task
  return $ Matrix handle (taskEvent task) w h ld 4

waitAndShow m = do
  eventWait (event m)
  putStrLn (show m)
  
-- |Register a ViperVM matrix a Float stored at the given address
floatMatrixRegister :: Ptr () -> Int -> Word -> Word -> Word -> IO Handle
floatMatrixRegister ptr node width height ld = alloca $ \handle -> do
  matrixRegister handle node ptr nld nx ny 4
  hdl <- peek handle
  newForeignPtr p_dataUnregisterLazy hdl
  where
    nld = fromIntegral ld
    nx = fromIntegral height
    ny = fromIntegral width

-- |Initialize a new matrix of Float using the given function
floatMatrixMayInit :: Maybe (Word -> Word -> Float) -> Word -> Word -> Matrix Float
floatMatrixMayInit f width height = unsafePerformIO $ do
  ptr <- starpuMalloc $ fromIntegral (width*height*4)
  case f of
    Nothing -> return ()
    Just g -> pokeArray (castPtr ptr) cells
      where
        cells = concat $ map (\col -> map (\row -> g col row) rows) cols
        rows = [0..height-1]
        cols = [0..width-1]
  handle <- floatMatrixRegister ptr 0 width height height
  return $ Matrix handle dummyEvent width height height 4

-- |Initialize a new matrix of Float using the given function
floatMatrixInit :: (Word -> Word -> Float) -> Word -> Word -> Matrix Float
floatMatrixInit f width height = floatMatrixMayInit (Just f) width height

-- |Matrix of Float containing invalid values
floatMatrixInvalid :: Word -> Word -> Matrix Float
floatMatrixInvalid width height = floatMatrixMayInit Nothing width height

floatMatrixRegisterInvalid :: Word -> Word -> IO Handle
floatMatrixRegisterInvalid width height = do
--  ptr <- starpuMalloc rawSize
  floatMatrixRegister nullPtr (-1) width height height
  where
    rawSize = fromIntegral (width*height*4)




withAcquiredData :: Data a => a -> (WordPtr -> IO b) -> IO b
withAcquiredData m f = do
  withForeignPtr (handle m) $ \hdl -> do
    eventWait (event m)
    dataAcquire hdl readOnly
    ptr <- matrixLocalPtr hdl
    res <- f (ptrToWordPtr ptr)
    dataRelease hdl
    return res

readFloatMatrix :: Matrix Float -> IO [[Float]]
readFloatMatrix m = withAcquiredData m $ \ptr -> do
  uptr <- return $ wordPtrToPtr ptr
  values <- mapM (\col -> (peekArray colSize (plusPtr uptr (colOffset col)))) cols
  return (Data.List.transpose values)
  where
    cols = [0..(width m) - 1]
    colOffset col = fromIntegral $ col * (ld m) * (elemSize m)
    colSize = fromIntegral $ (height m)


printFloatMatrix :: Matrix Float -> IO ()
printFloatMatrix m = do
  ms <- readFloatMatrix (floatMatrixTranspose m)
  putStrLn $ unlines $ map show ms
  return ()


{-# NOINLINE floatMatrixAdd #-}
{-# NOINLINE floatMatrixSub #-}
{-# NOINLINE floatMatrixMul #-}
{-# NOINLINE floatMatrixTranspose #-}
{-# NOINLINE floatMatrixScale #-}

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
