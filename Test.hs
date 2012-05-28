import Prelude hiding (mapM,foldl1)

import Data.Ix
import Data.Word
import Data.Traversable
import Data.Foldable
import Data.Time.Clock

import StarPU.Platform
import StarPU.DataTypes
import StarPU.Task

import StarPU.Data.Matrix
import StarPU.Data.FloatMatrix

import QR
import HighDataTypes
import System.IO

main = do
  putStrLn "Choose an example to run:"
  putStrLn "  1 - Reduction"
  putStrLn "  2 - Split Matrix Multiplication"
  putStrLn "  3 - Simple Matrix Multiplication (displayed)"
  putStrLn "  4 - Simple Matrix Addition (displayed)"
  putStrLn "  5 - Rewriting of Multiple Matrix Additions"
  putStr "> "
  hFlush stdout
  c <- getLine

  t0 <- getCurrentTime
  runtimeInit

  (t1,t2,t3) <- case read c of
    1 -> sample (matrixList 1024 1024)  reduction
    2 -> do
      (n,va,ha,vb,hb) <- selectSizes
      sample [floatMatrixSet n n 2.0, floatMatrixSet n n 3.0] (splitMatMult va ha vb hb)
    3 -> sample [identityMatrix 10, customMatrix 10 10] simpleMatMult
    4 -> sample [floatMatrixSet 10 5 2.0, floatMatrixSet 10 5 3.0] simpleMatAdd
    5 -> sample (map (floatMatrixSet 10 10) [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]) rewrittenMatAdd

  putStrLn "==============================================================="
  putStrLn $ "Runtime system initialisation time: " ++ show (diffUTCTime t1 t0)
  putStrLn $ "Data initialisation time: " ++ show (diffUTCTime t2 t1)
  putStrLn $ "Computing time: " ++ show (diffUTCTime t3 t2)
  putStrLn "==============================================================="


selectSizes :: IO (Word,Word,Word,Word,Word)
selectSizes = do
  putStr "Enter matrix size: "
  hFlush stdout
  n <- getLine
  putStr "Enter vertical split count for A: "
  hFlush stdout
  va <- getLine
  putStr "Enter horizontal split count for A: "
  hFlush stdout
  ha <- getLine
  putStr "Enter vertical split count for B: "
  hFlush stdout
  vb <- getLine
  putStr "Enter horizontal split count for B: "
  hFlush stdout
  hb <- getLine
  return (read n,read va,read ha, read hb, read vb)

identityMatrix n = floatMatrixInit (\x y -> if (x == y) then 1.0 else 0.0) n n
customMatrix n m = floatMatrixInit (\x y -> fromIntegral (10 + x*2 + y)) n m
matrixList n m = map (floatMatrixSet n m . fromIntegral) $ range (1, 30)

runtimeInit = do
  putStrLn "Initializing runtime system..."
  defaultInit
  cublasInit
  showRuntimeInfo

reduction ms = compute $ reduce (*) (HighVector ms)

splitMatMult va ha vb hb [a,b] = traverseHighMatrix compute $ highSGEMM (split va ha a) (split vb hb b)

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

rewrittenMatAdd [a,b,c,d,e,f,g,h,i,j] = do
  compute $ a + b + c + d + e + f + g + h + i + j

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
    dot v1 v2 = reduce (+) $ HighDataTypes.zipWith (*) v1 v2

