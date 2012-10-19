{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module HaskellPU.HighDataTypes where

import Prelude hiding (foldr)
import qualified Data.List
import Data.Traversable
import Data.Word
import Data.Ix
import Control.Applicative
import Data.Foldable
import HaskellPU.Data
import HaskellPU.Data.FloatMatrix
import HaskellPU.Data.Matrix

data HighVector a = HighVector [a] deriving Show
data HighMatrix a = HighMatrix [[a]] deriving Show

-- |Transpose a matrix
transpose :: HighMatrix a -> HighMatrix a
transpose (HighMatrix l) = HighMatrix (Data.List.transpose l)

-- |Get a vector of rows
rows :: HighMatrix a -> HighVector (HighVector a)
rows (HighMatrix l) = HighVector $ map HighVector l

-- |Get a vector of columns
columns :: HighMatrix a -> HighVector (HighVector a)
columns (HighMatrix l) = HighVector $ map HighVector (Data.List.transpose l)

class Zippable m where
  zipWith :: (a->b->c) -> m a -> m b -> m c

class Crossable m n | m -> n where
  crossWith :: (a->b->c) -> m a -> m b -> n c

instance Foldable HighVector where
	foldMap f (HighVector l) = foldMap f l

instance Foldable HighMatrix where
	foldMap f (HighMatrix l) = foldMap (foldMap f) l

instance Functor HighVector where
  fmap f (HighVector l) = HighVector $ fmap f l
  
instance Functor HighMatrix where
  fmap f (HighMatrix l) = HighMatrix $ fmap (fmap f) l

instance Crossable HighVector HighMatrix where
  -- |Perform a cartesian product of two vectors
  crossWith f (HighVector l1) (HighVector l2) = HighMatrix $ map (\x -> map (f x) l2) l1

instance Zippable HighVector where
  zipWith f (HighVector l1) (HighVector l2) = HighVector $ Data.List.zipWith f l1 l2

instance Zippable HighMatrix where
  zipWith f (HighMatrix l1) (HighMatrix l2) = HighMatrix $ map (uncurry (Data.List.zipWith f)) $ zip l1 l2

cross :: Crossable m n => m a  -> m b -> n (a,b)
cross a b = crossWith (,) a b

reduce :: (a -> a -> a) -> HighVector a -> a
reduce f (HighVector [])  = undefined
reduce f (HighVector [a]) = a
reduce f (HighVector xs) = xs `seq` reduce f (HighVector (inner xs))
  where
    inner [] = []
    inner [a] = [a]
    inner (a:b:xs) = [f a b] ++ inner xs

-- |Extract a row of a matrix
row :: Int -> HighMatrix a -> HighVector a
row i (HighMatrix m) = HighVector (m !! i)

-- |Extract a column of a matrix
column :: Int -> HighMatrix a -> HighVector a
column i m = row i (transpose m)

-- |Drop some top rows of a matrix
dropRows :: Int -> HighMatrix a -> HighMatrix a
dropRows i (HighMatrix l) = HighMatrix (drop i l)

-- |Drop some left columns of a matrix
dropColumns :: Int -> HighMatrix a -> HighMatrix a
dropColumns i m = transpose $ dropRows i $ transpose m

-- |Create a matrix from a vetor of rows
fromRows :: HighVector (HighVector a) -> HighMatrix a
fromRows (HighVector l) = HighMatrix $ map g l
  where
    g (HighVector l) = l

-- |Create a matrix from a vetor of columns
fromColumns :: HighVector (HighVector a) -> HighMatrix a
fromColumns = transpose . fromRows

head :: HighVector a -> a
head (HighVector l) = Data.List.head l

tail :: HighVector a -> HighVector a
tail (HighVector l) = HighVector $ Data.List.tail l


hheight :: HighMatrix a -> Int
hheight (HighMatrix m) = length m

hwidth :: HighMatrix a -> Int
hwidth m = hheight $ transpose m

cons :: a -> HighVector a -> HighVector a
cons a (HighVector l) = HighVector (a:l)

{- Force asynchronous computation of each cell of the high matrix -}
computeHighMatrix :: (Data a, Computable a) => HighMatrix a -> IO ()
computeHighMatrix a = traverseHighMatrix compute a

{- Wait for each cell of the high matrix to be computed -}
waitHighMatrix :: (Data a, Computable a) => HighMatrix a -> IO ()
waitHighMatrix a = traverseHighMatrix wait a

{- Compute in parallel each cell of the high matrix and wait for the result -}
computeHighMatrixSync :: (Data a, Computable a) => HighMatrix a -> IO ()
computeHighMatrixSync a = do
  computeHighMatrix a
  waitHighMatrix a

instance (Data a, Computable a) => Computable (HighMatrix a) where
  compute = computeHighMatrix
  wait = waitHighMatrix

printHighMatrix :: HighMatrix (Matrix Float) -> IO ()
printHighMatrix m = do
  computeHighMatrixSync m
  traverseHighMatrix printFloatMatrix m

split :: Word -> Word -> Matrix Float -> HighMatrix (Matrix Float)
split x y m = HighMatrix $ map (\r -> map (\c -> f c r) cols) rows
  where
    rows = [0..y-1]
    cols = [0..x-1]
    w = width m
    h = height m
    wp = div w x
    wr = w - (x*wp)
    hp = div h y
    hr = h - (y*hp)
    f c r = subMatrix (c*wp) (r*hp) myW myH m
      where
        myW = if c /= x-1 then wp else (wp+wr)
        myH = if r /= y-1 then hp else (hp+hr)

traverseHighMatrix :: (a -> IO ()) -> HighMatrix a -> IO ()
traverseHighMatrix g m = f 0 0
  where
    w = hwidth m
    h = hheight m
    HighMatrix r = m
    f x y = do
      if x >= w || y >= h
        then return ()
        else do g (r !! y !! x)
                if x == (w-1)
                  then do f 0 (y+1)
                  else do f (x+1) y

showHighMatrix :: HighMatrix (Matrix Float) -> IO ()
showHighMatrix = traverseHighMatrix waitAndShow

{-------------------
 - Rewrite Rules
 -------------------}

{-# RULES
"reduce_plus" forall x y z .  floatMatrixAdd (floatMatrixAdd x y) z = reduce floatMatrixAdd (HighVector [x,y,z])
"reduce_plus_add" forall xs y .  floatMatrixAdd (reduce floatMatrixAdd (HighVector xs)) y = reduce floatMatrixAdd (HighVector (xs ++ [y]))

"reduce_sub" forall x y z .  floatMatrixSub (floatMatrixSub x y) z = reduce floatMatrixSub (HighVector [x,y,z])
"reduce_sub_add" forall xs y .  floatMatrixSub (reduce floatMatrixSub (HighVector xs)) y = reduce floatMatrixSub (HighVector (xs ++ [y]))

"reduce_mul" forall x y z .  floatMatrixMul (floatMatrixMul x y) z = reduce floatMatrixMul (HighVector [x,y,z])
"reduce_mul_add" forall xs y .  floatMatrixMul (reduce floatMatrixMul (HighVector xs)) y = reduce floatMatrixMul (HighVector (xs ++ [y]))

"transpose_transpose" forall m . floatMatrixTranspose (floatMatrixTranspose m) = m

"scale_scale" forall f1 f2 m . floatMatrixScale f1 (floatMatrixScale f2 m) = floatMatrixScale (f1 * f2) m
  #-}

