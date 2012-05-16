{-# LANGUAGE ForeignFunctionInterface #-}

module StarPU.Platform where

import StarPU.Structures
import StarPU.Task
import Foreign.Ptr
import Foreign.C

{- StarPU's platform foreign functions -}

foreign import ccall unsafe "starpu.h starpu_init" initialize :: Ptr StarPUConf -> IO CInt
foreign import ccall unsafe "starpu.h starpu_shutdown" shutdown :: IO ()
foreign import ccall unsafe "starpu.h starpu_worker_get_count" workerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_combined_worker_get_count" combinedWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_cpu_worker_get_count" cpuWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_cuda_worker_get_count" cudaWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_spu_worker_get_count" spuWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_opencl_worker_get_count" openclWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_asynchronous_copy_disabled" asynchronousCopyDisabled :: CInt

foreign import ccall unsafe "starpu_util.h starpu_helper_cublas_init" cublasInit :: IO ()

foreign import ccall unsafe "starpu_data_set_default_sequential_consistency_flag" dataflowModeSet :: CUInt -> IO ()
foreign import ccall unsafe "starpu_data_get_default_sequential_consistency_flag" dataflowModeGet :: CUInt

{- Platform API -}

{- |Indicate if asynchronous copies are enabled -}
asynchronousCopyEnabled = (asynchronousCopyDisabled == 0)

{- |Indicate if StarPU's data flow mode is enabled -}
dataflowModeEnabled = (dataflowModeGet /= 0)

{- |Initialize StarPU to be used from Haskell -}
defaultInit = do
  initialize nullPtr
  dataflowModeSet 0

{- |Display platform info -}
showRuntimeInfo = putStrLn runtimeInfo

{- |Return platform info -}
runtimeInfo = foldl1 (\x y -> x ++ "\n" ++ y) infos
  where
    infos = [workers,combinedWorkers,cpuWorkers,cudaWorkers,openclWorkers,spuWorkers,async,dataflow]
    workers = "There are " ++ (show workerCount) ++ " workers"
    combinedWorkers = "There are " ++ (show combinedWorkerCount) ++ " combined workers"
    cpuWorkers = "There are " ++ (show cpuWorkerCount) ++ " cpu workers"
    cudaWorkers = "There are " ++ (show cudaWorkerCount) ++ " cuda workers"
    spuWorkers = "There are " ++ (show spuWorkerCount) ++ " spu workers"
    openclWorkers = "There are " ++ (show openclWorkerCount) ++ " opencl workers"
    async = "Asynchronous copy mechanism is " ++ (enabled asynchronousCopyEnabled)
    dataflow = "Dataflow mode is " ++ (enabled dataflowModeEnabled)
    enabled x = if x then "enabled" else "disabled"

{- |Compute the given parameter and wait for each task to complete -}
compute :: a -> IO a
compute a = do
  r <- return $! a
  taskWaitForAll
  return r
