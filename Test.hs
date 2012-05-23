import Prelude hiding (mapM,foldl1)
import Data.Ix
import Data.Traversable
import Data.Foldable

import StarPU.Platform
import StarPU.DataTypes
import StarPU.Task

import StarPU.Data.Matrix
import BLAS
import QR
import HighDataTypes

n = 16

m1 = floatMatrixInit (\x y -> 1.0) n n
m2 = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) n n
m3 = floatMatrixInit (\x y -> 2.0) n n
m4 = floatMatrixInit (\x y -> 3.0) n n

m5 = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) 10 10
m6 = floatMatrixInit (\x y -> fromIntegral (x + y)) 8 10

ms = map createMatrix $ range (1, 96)
  where
    createMatrix v = floatMatrixInit (\x y -> fromIntegral v) n n


main = do
  putStrLn "Initializing..."
  defaultInit
  cublasInit
  showRuntimeInfo

  putStrLn "Computing..."

  printFloatMatrix $ sgemm m2 m3

  putStrLn "A"
  printFloatMatrix m5

  putStrLn "B"
  printFloatMatrix m6

  putStrLn "A * B"
  printFloatMatrix $ sgemm m5 m6
  
  putStrLn "A (using blocks)"
  printHighMatrix (split 1 2 m5)

  putStrLn "B (using blocks)"
  printHighMatrix (split 2 1 m6)

  putStrLn "A * B (using blocks)"
  printHighMatrix $ highSGEMM (split 1 2 m5) (split 2 1 m6)

  putStrLn "Unregister matrices..."
  unregisterInvalid m1
  unregisterInvalid m2
  unregisterInvalid m3
  unregisterInvalid m4
  unregisterInvalid m5
  unregisterInvalid m6

  putStrLn "Shutting down..."
  shutdown



highSGEMM :: HighMatrix (Matrix Float) -> HighMatrix (Matrix Float) -> HighMatrix (Matrix Float)
highSGEMM m1 m2 = crossWith dot (rows m1) (columns m2)
  where
    dot v1 v2 = foldl1 matadd $ HighDataTypes.zipWith sgemm v1 v2

reduce :: (a -> a -> a) -> [a] -> a
reduce f []  = undefined
reduce f [a] = a
reduce f xs = foldl1 f (inner xs)
  where
    inner [] = []
    inner [a] = [a]
    inner (a:b:xs) = [f a b] ++ inner xs
