module ViperVM.Platform where

import ViperVM.Data
import ViperVM.Structures
import ViperVM.Task
import ViperVM.Event
import Foreign.Ptr
import Foreign.C
import Control.DeepSeq
import System.IO.Unsafe

{- ViperVM's platform foreign functions -}

foreign import ccall unsafe "starpu.h starpu_init" initialize :: Ptr ViperVMConf -> IO CInt
foreign import ccall unsafe "starpu.h starpu_shutdown" shutdown :: IO ()
foreign import ccall unsafe "starpu.h starpu_worker_get_count" workerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_combined_worker_get_count" combinedWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_cpu_worker_get_count" cpuWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_cuda_worker_get_count" cudaWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_spu_worker_get_count" spuWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_opencl_worker_get_count" openclWorkerCount :: CUInt
foreign import ccall unsafe "starpu.h starpu_asynchronous_copy_disabled" asynchronousCopyDisabled :: CInt

foreign import ccall unsafe "starpu_util.h starpu_cublas_init_v2" cublasInit :: IO ()

foreign import ccall unsafe "starpu_data_set_default_sequential_consistency_flag" dataflowModeSet :: CUInt -> IO ()
foreign import ccall unsafe "starpu_data_get_default_sequential_consistency_flag" dataflowModeGet :: CUInt
foreign import ccall unsafe "starpu_get_prefetch_flag" prefetchState :: Int
foreign import ccall unsafe "starpu_sched_policy_name" starpuSchedPolicyName :: CString
foreign import ccall unsafe "starpu_sched_policy_description" starpuSchedPolicyDescription :: CString

{- Platform API -}

{- |Indicate if asynchronous copies are enabled -}
asynchronousCopyEnabled = (asynchronousCopyDisabled == 0)

{- |Indicate if ViperVM's data flow mode is enabled -}
dataflowModeEnabled = (dataflowModeGet /= 0)

{- |Indicate if prefetching is enabled -}
prefetchEnabled = (prefetchState /= 0)

{- |Name of the active scheduling policy -}
schedPolicyName = unsafePerformIO $ do
  s <- peekCString $ starpuSchedPolicyName
  return s

{- |Description of the active scheduling policy -}
schedPolicyDescription = unsafePerformIO $ do
  s <- peekCString $ starpuSchedPolicyDescription
  return s

{- |Initialize ViperVM to be used from Haskell -}
defaultInit = do
  initialize nullPtr
  dataflowModeSet 0

{- |Display platform info -}
showRuntimeInfo = putStrLn runtimeInfo

{- |Return platform info -}
runtimeInfo = foldl1 (\x y -> x ++ "\n" ++ y) infos
  where
    infos = [workers,combinedWorkers,cpuWorkers,cudaWorkers,openclWorkers,spuWorkers,async,dataflow,sched,prefetch]
    workers = areIs workerCount "worker"
    combinedWorkers = areIs combinedWorkerCount "combined worker"
    cpuWorkers = areIs cpuWorkerCount "cpu worker"
    cudaWorkers = areIs cudaWorkerCount "cuda worker"
    spuWorkers = areIs spuWorkerCount "spu worker"
    openclWorkers = areIs openclWorkerCount "opencl worker"
    async = "Asynchronous copy mechanism is " ++ (enabled asynchronousCopyEnabled)
    dataflow = "Dataflow mode is " ++ (enabled dataflowModeEnabled)
    sched = "Active scheduling policy is " ++ schedPolicyName ++ " (" ++ schedPolicyDescription ++ ")"
    prefetch = "Prefetching is " ++ (enabled prefetchEnabled)
    enabled x = if x then "enabled" else "disabled"
    areIs x s = "There " ++ case x of
      0 -> "isn't any " ++ s
      1 -> "is a single " ++ s
      _ -> "are " ++ show x ++ " " ++ s ++ "s"

