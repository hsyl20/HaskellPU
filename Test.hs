import StarPU
import DataTypes
import HighDataTypes
import BLAS
import Task

import QR

main = do
  putStrLn "Initializing..."
  cublasInit
  defaultInit
  showRuntimeInfo
  putStrLn "Register matrices..."
  h1 <- starpuMalloc $ fromIntegral (128*128*128*4)
  h2 <- starpuMalloc $ fromIntegral (128*128*128*4)
  h3 <- starpuMalloc $ fromIntegral (128*128*128*4)
  m1 <- floatMatrixRegisterInvalid h1 128 128 128
  m2 <- floatMatrixRegisterInvalid h2 128 128 128
  m3 <- floatMatrixRegisterInvalid h3 128 128 128
  putStrLn (show m1)
  putStrLn (show m2)
  putStrLn (show m3)
  putStrLn "Performing SGEMM..."
  r <- return $! sgemm (sgemm m1 m2) m3
  putStrLn (show r)
  putStrLn "Wait for all tasks"
  taskWaitForAll
  putStrLn "Unregister matrices..."
  unregister m1
  unregister m2
  putStrLn "Shutting down..."
  shutdown
