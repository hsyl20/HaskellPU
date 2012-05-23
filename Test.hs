import Prelude hiding (mapM)
import Data.Ix
import Data.Traversable

import StarPU.Platform
import StarPU.DataTypes
import StarPU.Task

import StarPU.Data.Matrix
import BLAS
import QR
import HighDataTypes

n= 10

m1 = floatMatrixInit (\x y -> 1.0) n n
m2 = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) n n
m3 = floatMatrixInit (\x y -> 2.0) n n
m4 = floatMatrixInit (\x y -> 3.0) n n

ms = map createMatrix $ range (1, 30)
  where
    createMatrix v = floatMatrixInit (\x y -> fromIntegral v) n n

ms2 = zip msA msB
  where
--    msA = drop 1 ms
--    msB = take (length ms - 1) ms
    n = div (length ms) 2
    msA = drop n ms
    msB = take n ms


main = do
  putStrLn "Initializing..."
  defaultInit
  cublasInit
  showRuntimeInfo

  putStrLn "Computing..."

--  r <- compute $ sgemm (sgemm m1 m2) (sgemm m3 m4)
  printFloatMatrix m2 >>= putStrLn
--  r <- compute $ subMatrix 1 5 5 5 m2
--  printFloatMatrix r >>= putStrLn

--  r <- compute $ map (uncurry sgemm) ms2

  r <- compute $ split 3 2 m2

  printHighMatrix r

  putStrLn "Unregister matrices..."
  unregisterInvalid m1
  unregisterInvalid m2
  unregisterInvalid m3
  unregisterInvalid m4
--  unregisterInvalid r

  putStrLn "Shutting down..."
  shutdown
