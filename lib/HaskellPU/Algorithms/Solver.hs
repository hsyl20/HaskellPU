{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module HaskellPU.Algorithms.Solver where

class Solver a b x | a b -> x where
  solveAXB :: a -> b -> x
  solveXAB :: a -> b -> x

