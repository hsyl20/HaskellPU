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
import BLAS
import QR
import HighDataTypes
import IO

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
    1 -> sample (matrixList 1024 1024)  reduction
    2 -> do
      (n,va,ha,vb,hb) <- selectSizes
      sample [setMatrix n n 2.0, setMatrix n n 3.0] (splitMatMult va ha vb hb)
    3 -> sample [identityMatrix 10, customMatrix 10 10] simpleMatMult
    4 -> sample [setMatrix 10 5 2.0, setMatrix 10 5 3.0] simpleMatAdd

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
setMatrix n m value = floatMatrixInit (\x y -> value) n m
customMatrix n m = floatMatrixInit (\x y -> fromIntegral (10 + x + y)) 8 10
matrixList n m = map (setMatrix n m . fromIntegral) $ range (1, 30)

runtimeInit = do
  putStrLn "Initializing runtime system..."
  defaultInit
  cublasInit
  showRuntimeInfo

reduction ms = compute $ reduce sgemm (HighVector ms)

splitMatMult va ha vb hb [a,b] = traverseHighMatrix compute  $ highSGEMM (split va ha a) (split vb hb b)

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
    dot v1 v2 = reduce matadd $ HighDataTypes.zipWith sgemm v1 v2

