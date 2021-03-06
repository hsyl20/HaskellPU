import Prelude hiding (mapM,foldl1)

import Data.Word
import Data.Traversable
import Data.Time.Clock
import System.IO
import System.Exit

import HaskellPU.Platform
import HaskellPU.Data
import HaskellPU.Data.Matrix
import HaskellPU.HighDataTypes

import HaskellPU.Data.FloatMatrix
import HaskellPU.Data.TriangularMatrix

import HaskellPU.Algorithms.Solver
import HaskellPU.Algorithms.Cholesky


main :: IO ()
main = do
  putStrLn "Choose an example to run:"
  putStrLn "  1 - Reduction"
  putStrLn "  2 - Split Matrix Multiplication"
  putStrLn "  3 - Simple Matrix Multiplication (displayed)"
  putStrLn "  4 - Simple Matrix Addition (displayed)"
  putStrLn "  5 - Simple Matrix Transpose (displayed)"
  putStrLn "  6 - Simple Matrix Scale (displayed)"
  putStrLn "  7 - Rewriting of Multiple Matrix Additions"
  putStrLn "  8 - Triangular Matrix Solver (displayed)"
  putStrLn "  9 - Triangular Matrix Multiplication (displayed)"
  putStrLn "  10 - Cholesky (displayed)"
  putStrLn "  11 - Cholesky benchmark"
  putStrLn "  12 - Cholesky benchmark (already tiled)"
  putStr "> "
  hFlush stdout
  c <- getLine

  (initTime, dataInitTime, computeTime) <- case (read c :: Integer) of
    1 -> sample (matrixList 1024 1024)  reduction
    2 -> do
      n <- askNumber "Enter matrix size"
      va <- askNumber "Enter vertical split count for A"
      ha <- askNumber "Enter horizontal split count for A"
      vb <- askNumber "Enter vertical split count for B"
      hb <- askNumber "Enter horizontal split count for B"
      sample [floatMatrixSet n n 2.0, floatMatrixSet n n 3.0] (splitMatMult va ha vb hb)
    3 -> sample [floatMatrixScale 2.0 (identityMatrix 10), customMatrix 5 10] simpleMatMult
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
    12 -> do
      n <- askNumber "Enter matrix size"
      bsize <- askNumber "Enter block size"
      nb <-return $  div n bsize
      (initTime,dataInitTime,computeTime) <- sample [split nb nb (stableHilbertMatrix n)] choleskyBenchTiled
      putStrLn $ "Computing time: " ++ show (1000.0 * (toFloat computeTime)) ++ "ms"
      putStrLn $ "Synthetic GFlops: " ++ show (1.0*(fromIntegral n)*(fromIntegral n)*(fromIntegral n) / 3.0 / (toFloat computeTime) / 1000000000.0)
      return (initTime,dataInitTime,computeTime)
    _ -> exitSuccess
      

  putStrLn "==============================================================="
  putStrLn $ "Runtime system initialisation time: " ++ show initTime
  putStrLn $ "Data initialisation time: " ++ show dataInitTime
  putStrLn $ "Computing time: " ++ show computeTime
  putStrLn "==============================================================="

  shutdown

  where
    identityMatrix n = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) n n
    customMatrix n m = floatMatrixInit (\x y -> fromIntegral (10 + x*2 + y)) n m
    hilbertMatrix n = floatMatrixInit (\x y -> 1.0 / ((fromIntegral x) + (fromIntegral y) + 1.0)) n n
    stableHilbertMatrix n = hilbertMatrix n + (floatMatrixScale (fromIntegral n) (identityMatrix n))
    matrixList n m = map (floatMatrixSet n m . fromIntegral :: Int -> Matrix Float) [1..30]

    reduction ms = computeSync $ reduce (*) (HighVector ms)

    splitMatMult va ha vb hb [a,b] = do
      computeHighMatrixSync $ (split va ha a) * (split vb hb b)
    splitMatMult _ _ _ _ _ = undefined

    simpleMatMult [a,b] = do
      putStrLn "A"
      printFloatMatrix a
      putStrLn "B"
      printFloatMatrix b
      putStrLn "A * B"
      printFloatMatrix $ a * b
    simpleMatMult _ = undefined

    simpleMatAdd [a,b] = do
      putStrLn "A"
      printFloatMatrix a
      putStrLn "B"
      printFloatMatrix b
      putStrLn "A + B"
      printFloatMatrix $ a + b
    simpleMatAdd _ = undefined

    simpleMatTranspose [a] = do
      putStrLn "A"
      printFloatMatrix a
      putStrLn "A^T"
      printFloatMatrix $ floatMatrixTranspose a
    simpleMatTranspose _ = undefined

    simpleMatScale [a] = do
      putStrLn "A"
      printFloatMatrix a
      putStrLn "3.0 * 2.0 * A"
      printFloatMatrix $ floatMatrixScale 3.0 $ floatMatrixScale 2.0 a
    simpleMatScale _ = undefined

    rewrittenMatAdd [a,b,c,d,e,f,g,h,i,j] = do
      computeSync $ a + b + c + d + e + f + g + h + i + j
    rewrittenMatAdd _ = undefined

    simpleStrsm [am,b] = do
      putStrLn "A"
      a <- return $ LowerTriangularMatrix am False
      printTriangularFloatMatrix a
      putStrLn "B"
      printFloatMatrix b
      putStrLn "Solve A.X = B"
      printFloatMatrix $ solveAXB a b
    simpleStrsm _ = undefined

    simpleStrmm [am,b] = do
      putStrLn "A"
      a <- return $ LowerTriangularMatrix am False
      printTriangularFloatMatrix a
      putStrLn "B"
      printFloatMatrix b
      putStrLn "A.B"
      printFloatMatrix $ strmm 0 a b
    simpleStrmm _ = undefined

    choleskySample [a] = do
      putStrLn "A"
      printFloatMatrix a
      putStrLn "Cholesky A"
      printFloatMatrix $ floatMatrixPotrf a
    choleskySample _ = undefined

    choleskyBench bsize [a] = do
     computeSync (cholesky bsize a)
    choleskyBench _ _ = undefined

    choleskyBenchTiled [a] = do
     computeSync (choleskyTiled a)
    choleskyBenchTiled _ = undefined


toFloat :: Real a => a -> Float
toFloat n = realToFrac n :: Float

askNumber :: String -> IO Word
askNumber s = do
  putStr s
  putStr ": "
  hFlush stdout
  n <- getLine
  return (read n)

sample :: Computable a => [a] -> ([a] -> IO ()) -> IO (NominalDiffTime, NominalDiffTime, NominalDiffTime)
sample ds f = do
  putStrLn "Initializing runtime system..."
  t0 <- getCurrentTime
  defaultInit
  cublasInit
  showRuntimeInfo
  putStrLn "Initializing data..."
  t1 <- getCurrentTime
  _ <- mapM compute ds
  _ <- mapM wait ds
  putStrLn "Computing..."
  t2 <- getCurrentTime
  f ds
  t3 <- getCurrentTime
  putStrLn "Done."
  return (diffUTCTime t1 t0, diffUTCTime t2 t1, diffUTCTime t3 t2)




