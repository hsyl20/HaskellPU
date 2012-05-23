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

n = 128

m1 = floatMatrixInit (\x y -> 1.0) n n
m2 = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) n n
m3 = floatMatrixInit (\x y -> 2.0) n n
m4 = floatMatrixInit (\x y -> 3.0) n n

ms = map createMatrix $ range (1, 96)
  where
    createMatrix v = floatMatrixInit (\x y -> fromIntegral v) n n


main = do
  putStrLn "Initializing..."
  defaultInit
  cublasInit
  showRuntimeInfo

  putStrLn "Computing..."

--  printFloatMatrix $ sgemm (sgemm m1 m2) (sgemm m3 m4)
--  printFloatMatrix $ subMatrix 1 5 5 5 m2
--  compute $ reduce sgemm ms

--  printHighMatrix $ split 3 2 m2
  printFloatMatrix $ matadd m4 m3

  putStrLn "Unregister matrices..."
  unregisterInvalid m1
  unregisterInvalid m2
  unregisterInvalid m3
  unregisterInvalid m4

  putStrLn "Shutting down..."
  shutdown



highSGEMM :: HighMatrix (Matrix Float) -> HighMatrix (Matrix Float) -> HighMatrix (Matrix Float)
highSGEMM m1 m2 = crossWith dot (rows m1) (columns m2)
  where
    dot :: HighVector (Matrix Float) -> HighVector (Matrix Float) -> Matrix Float
    dot v1 v2 = foldl1 matadd $ HighDataTypes.zipWith sgemm v1 v2

reduce :: (a -> a -> a) -> [a] -> a
reduce f []  = undefined
reduce f [a] = a
reduce f xs = foldl1 f (inner xs)
  where
    inner [] = []
    inner [a] = [a]
    inner (a:b:xs) = [f a b] ++ inner xs
