module HighDataTypes where

import qualified Data.List

data HighVector a = HighVector [a]
data HighMatrix a = HighMatrix [[a]]

transpose :: HighMatrix a -> HighMatrix a
transpose (HighMatrix l) = HighMatrix (Data.List.transpose l)

rows :: HighMatrix a -> HighVector (HighVector a)
rows (HighMatrix l) = HighVector $ map HighVector l

columns :: HighMatrix a -> HighVector (HighVector a)
columns (HighMatrix l) = HighVector $ map HighVector (Data.List.transpose l)

class Zippable m where
  zipWith :: (a->b->c) -> m a -> m b -> m c

class Crossable m n | m -> n where
  crossWith :: (a->b->c) -> m a -> m b -> n c


instance Functor HighVector where
  fmap f (HighVector l) = HighVector $ fmap f l
  
instance Functor HighMatrix where
  fmap f (HighMatrix l) = HighMatrix $ fmap (fmap f) l

instance Crossable HighVector HighMatrix where
  crossWith f (HighVector l1) (HighVector l2) = HighMatrix $ map (\x -> map (f x) l2) l1

instance Zippable HighVector where
  zipWith f (HighVector l1) (HighVector l2) = HighVector $ Data.List.zipWith f l1 l2

instance Zippable HighMatrix where
  zipWith f (HighMatrix l1) (HighMatrix l2) = HighMatrix $ map (uncurry (Data.List.zipWith f)) $ zip l1 l2

cross :: Crossable m n => m a  -> m b -> n (a,b)
cross a b = crossWith (,) a b

row :: Int -> HighMatrix a -> HighVector a
row i (HighMatrix m) = HighVector (m !! i)

column :: Int -> HighMatrix a -> HighVector a
column i m = row i (transpose m)

dropRows :: Int -> HighMatrix a -> HighMatrix a
dropRows i (HighMatrix l) = HighMatrix (drop i l)

dropColumns :: Int -> HighMatrix a -> HighMatrix a
dropColumns i m = transpose $ dropRows i $ transpose m

fromRows :: HighVector (HighVector a) -> HighMatrix a
fromRows (HighVector l) = HighMatrix $ map g l
  where
    g (HighVector l) = l

fromColumns :: HighVector (HighVector a) -> HighMatrix a
fromColumns = transpose . fromRows

head :: HighVector a -> a
head (HighVector l) = Data.List.head l

tail :: HighVector a -> HighVector a
tail (HighVector l) = HighVector $ Data.List.tail l


height :: HighMatrix a -> Int
height (HighMatrix m) = length m

width :: HighMatrix a -> Int
width m = height $ transpose m

cons :: a -> HighVector a -> HighVector a
cons a (HighVector l) = HighVector (a:l)
