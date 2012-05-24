import Prelude hiding (mapM,foldl1)
import Data.Ix
import Data.Traversable
import Data.Foldable
import Data.Time.Clock

import StarPU.Platform
import StarPU.DataTypes
import StarPU.Task

import StarPU.Data.Matrix
import BLAS
import QR
import HighDataTypes

n = 4096
nb = 4

m1 = floatMatrixInit (\x y -> 1.0) n n
m2 = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) n n
m3 = floatMatrixInit (\x y -> 2.0) n n
m4 = floatMatrixInit (\x y -> 3.0) n n

m5 = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) 10 10
m6 = floatMatrixInit (\x y -> fromIntegral (x + y)) 8 10

ms = map createMatrix $ range (1, 30)
  where
    createMatrix v = floatMatrixInit (\x y -> fromIntegral v) n n


main = do
  putStrLn "Initializing runtime system..."
  t0 <- getCurrentTime
  defaultInit
  cublasInit
  showRuntimeInfo

  putStrLn "Initializing data..."
  t1 <- getCurrentTime
--  mapM compute ms
  mapM compute [m1,m2,m3,m4,m5,m6]

  putStrLn "Computing..."
  
  t2 <- getCurrentTime

{-  printFloatMatrix $ sgemm m2 m3

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
-}

  traverseHighMatrix compute  $ highSGEMM (split nb nb m3) (split nb nb m4)
--  compute $ reduce sgemm ms

  t3 <- getCurrentTime
  putStrLn "Done."
  
  putStrLn "==============================================================="
  putStrLn $ "Runtime system initialisation time: " ++ show (diffUTCTime t1 t0)
  putStrLn $ "Data initialisation time: " ++ show (diffUTCTime t2 t1)
  putStrLn $ "Computing time: " ++ show (diffUTCTime t3 t2)
  putStrLn "==============================================================="




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
