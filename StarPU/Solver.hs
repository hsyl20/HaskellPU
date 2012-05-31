module StarPU.Solver where

class Solver a b c | a b -> c where
  solve :: a -> b -> c

