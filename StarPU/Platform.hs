{-# LANGUAGE ForeignFunctionInterface #-}

module StarPU.Platform where

import StarPU.Structures
import DataTypes

import Foreign.Ptr
import Foreign.C
 
foreign import ccall unsafe "starpu.h starpu_init" initialize :: Ptr StarPUConf -> IO CInt
foreign import ccall unsafe "starpu.h starpu_shutdown" shutdown :: IO ()

defaultInit = initialize nullPtr

foreign import ccall unsafe "starpu.h starpu_worker_get_count" workerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_combined_worker_get_count" combinedWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_cpu_worker_get_count" cpuWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_cuda_worker_get_count" cudaWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_spu_worker_get_count" spuWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_opencl_worker_get_count" openclWorkerCount :: CUInt

foreign import ccall unsafe "starpu.h starpu_asynchronous_copy_disabled" starpu_asynchronous_copy_disabled :: CInt
asynchronousCopyEnabled = if (starpu_asynchronous_copy_disabled == 0) then True else False

foreign import ccall unsafe "starpu_util.h starpu_helper_cublas_init" cublasInit :: IO ()

showRuntimeInfo = putStrLn runtimeInfo

runtimeInfo = foldl1 (\x y -> x ++ "\n" ++ y) infos
  where
    infos = [workers,combinedWorkers,cpuWorkers,cudaWorkers,openclWorkers,spuWorkers,async]
    workers = "There are " ++ (show workerCount) ++ " workers"
    combinedWorkers = "There are " ++ (show combinedWorkerCount) ++ " combined workers"
    cpuWorkers = "There are " ++ (show cpuWorkerCount) ++ " cpu workers"
    cudaWorkers = "There are " ++ (show cudaWorkerCount) ++ " cuda workers"
    spuWorkers = "There are " ++ (show spuWorkerCount) ++ " spu workers"
    openclWorkers = "There are " ++ (show openclWorkerCount) ++ " opencl workers"
    async = "Asynchronous copy mechanism is " ++ (if asynchronousCopyEnabled then "enabled" else "disabled")

