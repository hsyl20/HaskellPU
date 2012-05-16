import Control.Monad

import StarPU.Platform
import StarPU.DataTypes
import StarPU.Task

import StarPU.Data.Matrix
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

  putStrLn "Computing..."

  r <- return $! sgemm (sgemm m1 m2) (sgemm m3 m4)

  putStrLn "Wait for all tasks"
  taskWaitForAll

  printFloatMatrix r >>= putStrLn

  putStrLn "Unregister matrices..."
  unregisterInvalid m1
  unregisterInvalid m2
  unregisterInvalid m3
  unregisterInvalid m4
  unregisterInvalid r

  putStrLn "Shutting down..."
  shutdown
