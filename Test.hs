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
import IO

n = 4096
nb = 4

m1 = floatMatrixInit (\x y -> 1.0) n n
m2 = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) n n
m3 = floatMatrixInit (\x y -> 2.0) n n
m4 = floatMatrixInit (\x y -> 3.0) n n

m5 = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) 10 10
m6 = floatMatrixInit (\x y -> fromIntegral (10 + x + y)) 8 10

ms = map createMatrix $ range (1, 30)
  where
    createMatrix v = floatMatrixInit (\x y -> fromIntegral v) n n


main = do
  putStrLn "Choose an example to run:"
  putStrLn "  1 - Reduction"
  putStrLn "  2 - Split Matrix Multiplication"
  putStrLn "  3 - Simple Matrix Multiplication (displayed)"
  putStrLn "  4 - Simple Matrix Addition (displayed)"
  putStr "> "
  hFlush stdout
  c <- getLine

  t0 <- getCurrentTime
  runtimeInit

  (t1,t2,t3) <- case read c of
    1 -> sample ms reduction
    2 -> sample [m3,m4] splitMatMult
    3 -> sample [m5,m6] simpleMatMult
    4 -> sample [m5,m6] simpleMatAdd

  putStrLn "==============================================================="
  putStrLn $ "Runtime system initialisation time: " ++ show (diffUTCTime t1 t0)
  putStrLn $ "Data initialisation time: " ++ show (diffUTCTime t2 t1)
  putStrLn $ "Computing time: " ++ show (diffUTCTime t3 t2)
  putStrLn "==============================================================="


runtimeInit = do
  putStrLn "Initializing runtime system..."
  defaultInit
  cublasInit
  showRuntimeInfo

reduction ms = compute $ reduce sgemm ms

splitMatMult [a,b] = traverseHighMatrix compute  $ highSGEMM (split nb nb a) (split nb nb b)

simpleMatMult [a,b] = do
  putStrLn "A"
  printFloatMatrix a

  putStrLn "B"
  printFloatMatrix b

  putStrLn "A * B"
  printFloatMatrix $ sgemm a b
  
simpleMatAdd [a,b] = do
  putStrLn "A"
  printFloatMatrix a

  putStrLn "B"
  printFloatMatrix b

  putStrLn "A + B"
  printFloatMatrix $ matadd a b

sample ds f = do
  putStrLn "Initializing data..."
  t1 <- getCurrentTime
  mapM compute ds

  putStrLn "Computing..."
  t2 <- getCurrentTime
  f ds

  t3 <- getCurrentTime
  putStrLn "Done."
  return (t1,t2,t3)

highSGEMM :: HighMatrix (Matrix Float) -> HighMatrix (Matrix Float) -> HighMatrix (Matrix Float)
highSGEMM m1 m2 = crossWith dot (rows m1) (columns m2)
  where
    dot v1 v2 = foldl1 matadd $ HighDataTypes.zipWith sgemm v1 v2

reduce :: (a -> a -> a) -> [a] -> a
reduce f []  = undefined
reduce f [a] = a
reduce f xs = xs `seq` reduce f (inner xs)
  where
    inner [] = []
    inner [a] = [a]
    inner (a:b:xs) = [f a b] ++ inner xs
