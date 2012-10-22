HaskellPU
=========

HaskellPU is a functional wrapper above StarPU (http://runtime.bordeaux.inria.fr/StarPU/).

* Dependencies
  * StarPU
  * CUDA/CuBlas
  * BLAS
  * Lapack
  * ghc
  * hsc2hs
  * gcc


$ cabal configure

$ cabal build

$ ./dist/build/Test/Test
