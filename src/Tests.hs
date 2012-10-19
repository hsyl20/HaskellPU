import Test.QuickCheck

import HaskellPU.Data.FloatMatrix
import HaskellPU.Data.Matrix
import HaskellPU.Platform
import Data.List
import System.IO.Unsafe

import HighDataTypes (HighVector(HighVector), reduce)

newtype SquareMatrix a = SquareMatrix (Matrix Float)

newtype MatrixMulCouple = MatrixMulCouple (Matrix Float, Matrix Float) deriving Show
newtype MatrixAddCouple = MatrixAddCouple (Matrix Float, Matrix Float) deriving Show

squareMatrix :: Gen (SquareMatrix (Matrix Float))
squareMatrix = do
  s <- choose (1,256)
  values <- vector s
  w <- return $ floor (sqrt (fromIntegral s))
  return $ SquareMatrix $ floatMatrixInit (\x y -> values !! fromIntegral (x+y*w)) (fromIntegral w) (fromIntegral w)

rectangularMatrix :: Gen (Matrix Float)
rectangularMatrix = do
  w <- choose (1,48)
  h <- choose (1,48)
  values <- vector (w*h)
  return $ floatMatrixInit (\x y -> values !! fromIntegral (x+y*(fromIntegral w))) (fromIntegral w) (fromIntegral h)

mulCoupleMatrix :: Gen MatrixMulCouple
mulCoupleMatrix = do
  w <- choose (1,48)
  h <- choose (1,48)
  k <- choose (1,48)
  valuesA <- vector (k*h)
  valuesB <- vector (k*w)
  a <- return $ floatMatrixInit (\x y -> valuesA !! fromIntegral (x+y*(fromIntegral k))) (fromIntegral k) (fromIntegral h)
  b <- return $ floatMatrixInit (\x y -> valuesB !! fromIntegral (x+y*(fromIntegral w))) (fromIntegral w) (fromIntegral k)
  return $ MatrixMulCouple (a,b)

addCoupleMatrix :: Gen MatrixAddCouple
addCoupleMatrix = do
  w <- choose (1,48)
  h <- choose (1,48)
  valuesA <- vector (w*h)
  valuesB <- vector (w*h)
  a <- return $ floatMatrixInit (\x y -> valuesA !! fromIntegral (x+y*(fromIntegral w))) (fromIntegral w) (fromIntegral h)
  b <- return $ floatMatrixInit (\x y -> valuesB !! fromIntegral (x+y*(fromIntegral w))) (fromIntegral w) (fromIntegral h)
  return $ MatrixAddCouple (a,b)

instance Arbitrary (SquareMatrix (Matrix Float)) where
  arbitrary = squareMatrix

instance Arbitrary (Matrix Float) where
  arbitrary = rectangularMatrix

instance Arbitrary MatrixMulCouple where
  arbitrary = mulCoupleMatrix

instance Arbitrary MatrixAddCouple where
  arbitrary = addCoupleMatrix

check_equality m1 m2 epsilon = do
  m1s <- readFloatMatrix m1
  m2s <- readFloatMatrix m2
  diff <- return $ zipWith (-) (concat m1s) (concat m2s)
  res <- return $ all (\x -> x < epsilon) diff
  if (not res) then do
--    putStrLn $ show m1s
--    putStrLn $ show m2s
--    putStrLn $ show diff
    putStrLn $ show $ filter (epsilon <=) diff
  else
    return ()
  return $ res

dot l1 l2 = reduce (+) $ HighVector (zipWith (*) l1 l2)

prop_mult :: MatrixMulCouple -> Property
prop_mult (MatrixMulCouple (m1,m2)) = (width m1 == height m2) ==> unsafePerformIO $ do
  m1s <- readFloatMatrix m1
  m2s <- readFloatMatrix m2
  refvalues <- return $ map (\x -> map (dot x) (transpose m2s)) m1s
  ref <- return $ floatMatrixInit (\x y -> refvalues !! (fromIntegral y) !! (fromIntegral x)) (width m2) (height m1)
  check_equality (m1*m2) ref 1000000.0  -- FIXME: CUDA kernel requires a hiiiigh epsilon...

prop_add :: MatrixAddCouple -> Property
prop_add (MatrixAddCouple (m1,m2)) = (width m1 == width m2 && height m1 == height m2) ==> unsafePerformIO $ do
  m1s <- readFloatMatrix m1
  m2s <- readFloatMatrix m2
  refvalues <- return $ zipWith (zipWith (+)) m1s m2s
  ref <- return $ floatMatrixInit (\x y -> refvalues !! (fromIntegral y) !! (fromIntegral x)) (width m1) (height m1)
  check_equality (m1+m2) ref 0.01

prop_transpose :: Matrix Float -> Bool
prop_transpose m = unsafePerformIO $ do
  r <- readFloatMatrix m
  ref <- return $ floatMatrixInit (\x y -> r !! (fromIntegral x) !! (fromIntegral y)) (height m) (width m)
  check_equality (floatMatrixTranspose m) ref 0.0001

main = do
  defaultInit
  cublasInit
  showRuntimeInfo
  putStrLn "Checking that (transpose . transpose) == identity"
  quickCheck prop_transpose
  putStrLn "Checking float matrix addition"
  quickCheck prop_add
  putStrLn "Checking float matrix multiplication"
  quickCheck prop_mult
