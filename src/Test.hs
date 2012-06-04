import Prelude hiding (mapM,foldl1)

import Data.Ix
import Data.Word
import Data.Traversable
import Data.Foldable
import Data.Time.Clock

import StarPU.Platform
import StarPU.Task

import StarPU.Data.Matrix
import StarPU.Data.FloatMatrix
import StarPU.Data.TriangularMatrix
import StarPU.Solver

import QR
import Cholesky
import HighDataTypes
import System.IO


main = do
  putStrLn "Choose an example to run:"
  putStrLn "  1 - Reduction"
  putStrLn "  2 - Split Matrix Multiplication"
  putStrLn "  3 - Simple Matrix Multiplication (displayed)"
  putStrLn "  4 - Simple Matrix Addition (displayed)"
  putStrLn "  5 - Simple Matrix Tranpose (displayed)"
  putStrLn "  6 - Simple Matrix Scale (displayed)"
  putStrLn "  7 - Rewriting of Multiple Matrix Additions"
  putStrLn "  8 - Triangular Matrix Solver (displayed)"
  putStrLn "  9 - Triangular Matrix Multiplication (displayed)"
  putStrLn "  10 - Cholesky (displayed)"
  putStrLn "  11 - Cholesky benchmark"
  putStr "> "
  hFlush stdout
  c <- getLine

  (initTime, dataInitTime, computeTime) <- case read c of
    1 -> sample (matrixList 1024 1024)  reduction
    2 -> do
      n <- askNumber "Enter matrix size"
      va <- askNumber "Enter vertical split count for A"
      ha <- askNumber "Enter horizontal split count for A"
      vb <- askNumber "Enter vertical split count for B"
      hb <- askNumber "Enter horizontal split count for B"
      sample [floatMatrixSet n n 2.0, floatMatrixSet n n 3.0] (splitMatMult va ha vb hb)
    3 -> sample [floatMatrixScale 2.0 (identityMatrix 10), customMatrix 10 10] simpleMatMult
    4 -> sample [floatMatrixSet 10 5 2.0, floatMatrixSet 10 5 3.0] simpleMatAdd
    5 -> sample [customMatrix 15 10] simpleMatTranspose
    6 -> sample [customMatrix 10 5] simpleMatScale
    7 -> sample (map (floatMatrixSet 10 10) [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]) rewrittenMatAdd
    8 -> sample [floatMatrixScale 2.0 (identityMatrix 10), customMatrix 10 10] simpleStrsm
    9 -> sample [floatMatrixScale 2.0 (identityMatrix 10), customMatrix 10 10] simpleStrmm
    10 -> sample [stableHilbertMatrix 16] choleskySample
    11 -> do
      n <- askNumber "Enter matrix size"
      b <- askNumber "Enter block size"
      (initTime,dataInitTime,computeTime) <- sample [stableHilbertMatrix n] (choleskyBench b)
      putStrLn $ "Computing time: " ++ show (1000.0 * (toFloat computeTime)) ++ "ms"
      putStrLn $ "Synthetic GFlops: " ++ show (1.0*(fromIntegral n)*(fromIntegral n)*(fromIntegral n) / 3.0 / (toFloat computeTime) / 1000000000.0)
      return (initTime,dataInitTime,computeTime)
      

  putStrLn "==============================================================="
  putStrLn $ "Runtime system initialisation time: " ++ show initTime
  putStrLn $ "Data initialisation time: " ++ show dataInitTime
  putStrLn $ "Computing time: " ++ show computeTime
  putStrLn "==============================================================="


toFloat n = realToFrac n :: Float

askNumber :: String -> IO Word
askNumber s = do
  putStr s
  putStr ": "
  hFlush stdout
  n <- getLine
  return (read n)

identityMatrix n = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) n n
customMatrix n m = floatMatrixInit (\x y -> fromIntegral (10 + x*2 + y)) n m
hilbertMatrix n = floatMatrixInit (\x y -> 1.0 / ((fromIntegral x) + (fromIntegral y) + 1.0)) n n
stableHilbertMatrix n = hilbertMatrix n + (floatMatrixScale (fromIntegral n) (identityMatrix n))
matrixList n m = map (floatMatrixSet n m . fromIntegral) $ range (1, 30)

sample ds f = do
  putStrLn "Initializing runtime system..."
  t0 <- getCurrentTime
  defaultInit
  cublasInit
  showRuntimeInfo
  putStrLn "Initializing data..."
  t1 <- getCurrentTime
  mapM compute ds
  putStrLn "Computing..."
  t2 <- getCurrentTime
  f ds
  t3 <- getCurrentTime
  putStrLn "Done."
  return (diffUTCTime t1 t0, diffUTCTime t2 t1, diffUTCTime t3 t2)


reduction ms = compute $ reduce (*) (HighVector ms)

splitMatMult va ha vb hb [a,b] = do
  traverseHighMatrix compute $ highSGEMM (split va ha a) (split vb hb b)

simpleMatMult [a,b] = do
  putStrLn "A"
  printFloatMatrix a
  putStrLn "B"
  printFloatMatrix b
  putStrLn "A * B"
  printFloatMatrix $ a * b

simpleMatAdd [a,b] = do
  putStrLn "A"
  printFloatMatrix a
  putStrLn "B"
  printFloatMatrix b
  putStrLn "A + B"
  printFloatMatrix $ a + b

simpleMatTranspose [a] = do
  putStrLn "A"
  printFloatMatrix a
  putStrLn "A^T"
  printFloatMatrix $ floatMatrixTranspose a

simpleMatScale [a] = do
  putStrLn "A"
  printFloatMatrix a
  putStrLn "3.0 * 2.0 * A"
  printFloatMatrix $ floatMatrixScale 3.0 $ floatMatrixScale 2.0 a

rewrittenMatAdd [a,b,c,d,e,f,g,h,i,j] = do
  compute $ a + b + c + d + e + f + g + h + i + j

simpleStrsm [am,b] = do
  putStrLn "A"
  a <- return $ LowerTriangularMatrix am False
  printTriangularFloatMatrix a
  putStrLn "B"
  printFloatMatrix b
  putStrLn "Solve A.X = B"
  printFloatMatrix $ solveAXB a b

simpleStrmm [am,b] = do
  putStrLn "A"
  a <- return $ LowerTriangularMatrix am False
  printTriangularFloatMatrix a
  putStrLn "B"
  printFloatMatrix b
  putStrLn "A.B"
  printFloatMatrix $ strmm 0 a b

choleskySample [a] = do
  putStrLn "A"
  printFloatMatrix a
  putStrLn "Cholesky A"
  printFloatMatrix $ floatMatrixPotrf a

choleskyBench bsize [a] = do
 traverseHighMatrix compute $ cholesky bsize a

highSGEMM :: HighMatrix (Matrix Float) -> HighMatrix (Matrix Float) -> HighMatrix (Matrix Float)
highSGEMM m1 m2 = crossWith dot (rows m1) (columns m2)
  where
    dot v1 v2 = reduce (+) $ HighDataTypes.zipWith (*) v1 v2
