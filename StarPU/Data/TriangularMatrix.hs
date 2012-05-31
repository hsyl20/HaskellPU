module StarPU.Data.TriangularMatrix where

import StarPU.Solver
import StarPU.Task
import StarPU.Data
import StarPU.Data.FloatMatrix
import StarPU.Data.Matrix

{-| A triangular matrix can either be lower or upper. It is based on a regular
    matrix and must indicate if it is a unit matrix -}
data TriangularMatrix a = LowerTriangularMatrix (Matrix a) Bool | UpperTriangularMatrix (Matrix a) Bool

foreign import ccall unsafe "floatmatrix_strsm_task_create" floatMatrixStrsmTaskCreate :: Int -> Int -> Int -> Handle -> Handle -> Handle -> Task

{-------------------
 - Operations
 -------------------}
strsmAXB :: TriangularMatrix Float -> Matrix Float -> Matrix Float
strsmAXB a b = floatMatrixBinOp f m b (width b) (height b)
  where
    (uplo, unit, m) = case a of
      LowerTriangularMatrix m unit -> (0,unit,m)
      UpperTriangularMatrix m unit -> (1,unit,m)
    f = floatMatrixStrsmTaskCreate uplo (if unit then 0 else 1) 0

zipWithIndex :: [a] -> [(a,Int)]
zipWithIndex l = inner l 0
  where
    inner (x:xs) i = (x,i):(inner xs (i+1))
    inner [] i = []

printTriangularFloatMatrix :: TriangularMatrix Float -> IO ()
printTriangularFloatMatrix (LowerTriangularMatrix m _) = do
  ms <- readFloatMatrix m
  putStrLn $ unlines $ map show $ map (\(xs,i) -> take (i+1) xs) $ zipWithIndex ms
  return ()
