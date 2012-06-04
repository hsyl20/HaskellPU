module StarPU.Data where

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

import StarPU.Structures
import StarPU.Event
import StarPU.Task
import StarPU.AccessMode


type Handle = ForeignPtr ()
type UnsafeHandle = Ptr ()

class Data a where
  handle :: a -> Handle
  event :: a -> Event

foreign import ccall "starpu_data_unregister" dataUnregister :: UnsafeHandle -> IO ()
foreign import ccall "starpu_data_unregister_no_coherency" dataUnregisterInvalid :: UnsafeHandle -> IO ()
foreign import ccall "starpu_data_unregister_lazy" dataUnregisterLazy :: UnsafeHandle -> IO ()
foreign import ccall "&starpu_data_unregister_lazy" p_dataUnregisterLazy :: FunPtr(UnsafeHandle -> IO ())
foreign import ccall "starpu_data_invalidate" dataInvalidate :: UnsafeHandle -> IO ()
foreign import ccall "starpu_data_release" dataRelease :: UnsafeHandle -> IO ()

foreign import ccall "starpu_data_acquire" dataAcquire :: UnsafeHandle -> AccessMode -> IO Int
foreign import ccall unsafe "starpu_malloc_ex" starpuMalloc :: CSize -> IO (Ptr ())

unregister :: Data a => a -> IO ()
unregister a = withForeignPtr (handle a) $ dataUnregister

unregisterInvalid :: Data a => a -> IO ()
unregisterInvalid a = withForeignPtr (handle a) $ dataUnregisterInvalid

invalidate :: Data a => a -> IO ()
invalidate a = withForeignPtr (handle a) $ dataInvalidate

release :: Data a => a -> IO ()
release a = withForeignPtr (handle a) $ dataRelease

acquire :: Data a => AccessMode -> a -> IO Int
acquire mode a = withForeignPtr (handle a) $ \hdl -> dataAcquire hdl mode
