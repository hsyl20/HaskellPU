{-# LANGUAGE ForeignFunctionInterface #-}
 
module HaskellPU.Structures where
 
import Foreign
import Foreign.C.Types
 
#include <starpu.h>
 
data HaskellPUConf = HaskellPUConf {
  ncpus :: CInt,
  ncuda :: CInt,
  nopencl :: CInt,
  nspus :: CInt
}
 
instance Storable HaskellPUConf where
    sizeOf    _ = (#size struct starpu_conf)
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        cpus <- (#peek struct starpu_conf, ncpus) ptr
        cuda <- (#peek struct starpu_conf, ncuda) ptr
        opencl <- (#peek struct starpu_conf, nopencl) ptr
        spus <- (#peek struct starpu_conf, nspus) ptr
        return  HaskellPUConf { 
            ncpus = cpus,
            ncuda = cuda,
            nopencl = opencl,
            nspus = spus
        }
    poke ptr (HaskellPUConf ncpusV ncudaV nopenclV nspusV) = do
        (#poke struct starpu_conf, ncpus) ptr ncpusV
        (#poke struct starpu_conf, ncuda) ptr ncudaV
        (#poke struct starpu_conf, nopencl) ptr nopenclV
        (#poke struct starpu_conf, nspus) ptr nspusV
