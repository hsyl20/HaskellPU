module HighDataTypes where

import Prelude hiding (foldr)
import qualified Data.List
import Data.Traversable
import Control.Applicative
import Data.Foldable

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
