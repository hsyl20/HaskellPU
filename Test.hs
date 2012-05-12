import StarPU
import DataTypes
import HighDataTypes
import BLAS
import Task

import QR

m1 = floatMatrixInit (\x y -> 1.0) 128 128
m2 = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) 128 128
m3 = floatMatrixInit (\x y -> 2.0) 128 128

main = do
  putStrLn "Initializing..."
  cublasInit
  defaultInit
  showRuntimeInfo
  putStrLn "Register matrices..."
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
  unregister m3
  unregister r
  putStrLn "Shutting down..."
  shutdown
