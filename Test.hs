import StarPU.Platform
import StarPU.DataTypes
import StarPU.Task

import HighDataTypes

import BLAS.SGEMM
import QR

n= 10

m1 = floatMatrixInit (\x y -> 1.0) n n
m2 = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) n n
m3 = floatMatrixInit (\x y -> 2.0) n n
m4 = floatMatrixInit (\x y -> 3.0) n n

main = do
  putStrLn "Initializing..."
  defaultInit
  cublasInit
  showRuntimeInfo
  putStrLn "Register matrices..."
  putStrLn (show m1)
  putStrLn (show m2)
  putStrLn (show m3)
  putStrLn (show m4)
  putStrLn "Performing SGEMM..."
  r <- return $ sgemm (sgemm m1 m2) (sgemm m3 m4)
  putStrLn "Wait for all tasks"
  taskWaitForAll
  putStrLn (show r)
  result <- readFloatMatrix r
  putStrLn (show result)
  putStrLn "Unregister matrices..."
  unregister m1
  unregister m2
  unregister m3
  unregister m4
  unregister r
  putStrLn "Shutting down..."
  shutdown
