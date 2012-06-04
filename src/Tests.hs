import Test.QuickCheck

import StarPU.Data.FloatMatrix
import StarPU.Data.Matrix
import StarPU.Platform
import Data.List
import System.IO.Unsafe

squareMatrix :: Gen (Matrix Float)
squareMatrix = do
  s <- choose (1,256)
  values <- vector s
  w <- return $ round (sqrt (fromIntegral s))
  return $ floatMatrixInit (\x y -> values !! fromIntegral ((x+y*w))) (fromIntegral (w-1)) (fromIntegral (w-1))

instance Arbitrary (Matrix Float) where
  arbitrary = squareMatrix

dot l1 l2 = foldl1 (+) $ zipWith (*) l1 l2

mult m1 m2 = unsafePerformIO $ do
  m1s <- readFloatMatrix m1
  m2s <- readFloatMatrix m2
  refvalues <- return $ map (\x -> map (dot x) (transpose m2s)) m1s
  ref <- return $ floatMatrixInit (\x y -> refvalues !! (fromIntegral x) !! (fromIntegral y)) (width m2) (height m1)
  check_equality (m1*m2) ref

add m1 m2 = unsafePerformIO $ do
  m1s <- readFloatMatrix m1
  m2s <- readFloatMatrix m2
  refvalues <- return $ zipWith (zipWith (+)) m1s m2s
  ref <- return $ floatMatrixInit (\x y -> refvalues !! (fromIntegral x) !! (fromIntegral y)) (width m1) (height m1)
  check_equality (m1+m2) ref

check_equality m1 m2 = do
  m1s <- readFloatMatrix m1
  m2s <- readFloatMatrix m2
  diff <- return $ zipWith (-) (concat m1s) (concat m2s)
  return $ all (\x -> x < 0.1) diff

prop_mult :: Matrix Float -> Matrix Float -> Property
prop_mult m1 m2 = (width m1 == height m2) ==> mult m1 m2

prop_add :: Matrix Float -> Matrix Float -> Property
prop_add m1 m2 = (width m1 == width m2 && height m1 == height m2) ==> add m1 m2

prop_transpose :: Matrix Float -> Bool
prop_transpose m = unsafePerformIO $ do
  r <- return $ floatMatrixTranspose m
  r2 <- return $ floatMatrixTranspose r
  check_equality r2 m

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
