{-# LANGUAGE ForeignFunctionInterface #-}
 
module Structures where
 
import Event

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
 
#include <starpu.h>
 
data StarPUConf = StarPUConf {
  ncpus :: CInt,
  ncuda :: CInt,
  nopencl :: CInt,
  nspus :: CInt
}
 
instance Storable StarPUConf where
    sizeOf    _ = (#size struct starpu_conf)
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        cpus <- (#peek struct starpu_conf, ncpus) ptr
        cuda <- (#peek struct starpu_conf, ncuda) ptr
        opencl <- (#peek struct starpu_conf, nopencl) ptr
        spus <- (#peek struct starpu_conf, nspus) ptr
        return  StarPUConf { 
            ncpus = cpus,
            ncuda = cuda,
            nopencl = opencl,
            nspus = spus
        }
    poke ptr (StarPUConf ncpus ncuda nopencl nspus) = do
        (#poke struct starpu_conf, ncpus) ptr ncpus
        (#poke struct starpu_conf, ncuda) ptr ncuda
        (#poke struct starpu_conf, nopencl) ptr nopencl
        (#poke struct starpu_conf, nspus) ptr nspus

type Handle = Ptr ()

class Data a where
  handle :: a -> Handle
  event :: a -> Event


data FloatMatrix = FloatMatrix {
	floatMatrixHandle :: Handle,
  floatMatrixEvent :: Event,
	nx :: CUInt,
	ny :: CUInt,
	ld :: CUInt,
	elemSize :: CUInt
}

instance Data FloatMatrix where
  handle = floatMatrixHandle
  event = floatMatrixEvent
