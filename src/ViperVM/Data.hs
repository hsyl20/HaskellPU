module ViperVM.Data where

import Foreign.Ptr
import Data.List
import Data.Word
import Data.Ix
import Control.Monad
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.IO.Unsafe

import ViperVM.Structures
import ViperVM.Event
import ViperVM.Task
import ViperVM.AccessMode


type Handle = ForeignPtr ()
type UnsafeHandle = Ptr ()

class Data a where
  handle :: a -> Handle
  event :: a -> Event

foreign import ccall unsafe "starpu_data_unregister" dataUnregister :: UnsafeHandle -> IO ()
foreign import ccall unsafe "starpu_data_unregister_no_coherency" dataUnregisterInvalid :: UnsafeHandle -> IO ()
foreign import ccall unsafe "starpu_data_unregister_submit" dataUnregisterLazy :: UnsafeHandle -> IO ()
foreign import ccall unsafe "&starpu_data_unregister_submit" p_dataUnregisterLazy :: FunPtr(UnsafeHandle -> IO ())
foreign import ccall unsafe "starpu_data_invalidate" dataInvalidate :: UnsafeHandle -> IO ()
foreign import ccall unsafe "starpu_data_release" dataRelease :: UnsafeHandle -> IO ()
foreign import ccall unsafe "starpu_data_duplicate_ex" dataDuplicate :: UnsafeHandle -> UnsafeHandle -> IO Task

foreign import ccall unsafe "starpu_data_acquire" dataAcquire :: UnsafeHandle -> AccessMode -> IO Int
foreign import ccall unsafe "starpu_malloc_ex" starpuMalloc :: CSize -> IO (Ptr ())

foreign import ccall unsafe "force_compute" dataForceCompute :: UnsafeHandle -> IO ()

unregister :: Data a => a -> IO ()
unregister a = withForeignPtr (handle a) $ dataUnregister

unregisterInvalid :: Data a => a -> IO ()
unregisterInvalid a = withForeignPtr (handle a) $ dataUnregisterInvalid

invalidate :: Data a => a -> IO ()
invalidate a = withForeignPtr (handle a) $ dataInvalidate

class Computable a where
  {- |Force asynchronous computation of the given parameter -}
  compute :: a -> IO ()

  {- |Synchronous wait for a data -}
  wait :: a -> IO ()

  {- |Force synchronous computation of the given parameter -}
  computeSync :: a -> IO ()
  computeSync a = do
    compute a
    wait a
