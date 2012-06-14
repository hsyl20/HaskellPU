module ViperVM.Data.TriangularMatrix where

import ViperVM.Solver
import ViperVM.Task
import ViperVM.Data
import ViperVM.Data.FloatMatrix
import ViperVM.Data.Matrix

{-| A triangular matrix can either be lower or upper. It is based on a regular
    matrix and must indicate if it is a unit matrix -}
data TriangularMatrix a = LowerTriangularMatrix (Matrix a) Bool | UpperTriangularMatrix (Matrix a) Bool

foreign import ccall unsafe "floatmatrix_strsm_task_create" floatMatrixStrsmTaskCreate :: Int -> Bool -> Int -> UnsafeHandle -> UnsafeHandle -> UnsafeHandle -> IO Task
foreign import ccall unsafe "floatmatrix_strmm_task_create" floatMatrixStrmmTaskCreate :: Int -> Bool -> Int -> UnsafeHandle -> UnsafeHandle -> UnsafeHandle -> IO Task
foreign import ccall unsafe "floatmatrix_ssyrk_task_create" floatMatrixSsyrkTaskCreate :: Bool -> Bool -> UnsafeHandle -> UnsafeHandle -> IO Task

{-------------------
 - Operations
 -------------------}
strsm :: Int -> TriangularMatrix Float -> Matrix Float -> Matrix Float
strsm side a b = floatMatrixBinOp f m b (width b) (height b)
  where
    (uplo, unit, m) = case a of
      LowerTriangularMatrix m unit -> (0,unit,m)
      UpperTriangularMatrix m unit -> (1,unit,m)
    f = floatMatrixStrsmTaskCreate uplo unit side

strmm :: Int -> TriangularMatrix Float -> Matrix Float -> Matrix Float
strmm side a b = floatMatrixBinOp f m b w h
  where
    w = if side == 0 then width b else width m
    h = if side == 0 then height m else height b
    (uplo, unit, m) = case a of
      LowerTriangularMatrix m unit -> (0,unit,m)
      UpperTriangularMatrix m unit -> (1,unit,m)
    f = floatMatrixStrmmTaskCreate uplo unit side

ssyrk :: Bool -> TriangularMatrix Float -> Matrix Float
ssyrk trans a = floatMatrixUnaryOp f m w h
  where
    w = if trans then height m else width m
    h = if trans then width m else height m
    (uplo, unit, m) = case a of
      LowerTriangularMatrix m unit -> (False,unit,m)
      UpperTriangularMatrix m unit -> (True,unit,m)
    f = floatMatrixSsyrkTaskCreate uplo trans

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

{-------------------
 - Instances
 -------------------}
instance Solver (TriangularMatrix Float) (Matrix Float) (Matrix Float) where
  solveAXB = strsm 0
  solveXAB = strsm 1

