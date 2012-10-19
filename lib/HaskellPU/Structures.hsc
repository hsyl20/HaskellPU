{-# LANGUAGE ForeignFunctionInterface #-}
 
module HaskellPU.Structures where
 
import HaskellPU.Event

import Foreign
import Foreign.Ptr
import Foreign.C.String
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
    poke ptr (HaskellPUConf ncpus ncuda nopencl nspus) = do
        (#poke struct starpu_conf, ncpus) ptr ncpus
        (#poke struct starpu_conf, ncuda) ptr ncuda
        (#poke struct starpu_conf, nopencl) ptr nopencl
        (#poke struct starpu_conf, nspus) ptr nspus
