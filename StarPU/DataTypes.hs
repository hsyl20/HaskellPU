module StarPU.DataTypes where

import StarPU.Structures
import StarPU.Event
import StarPU.Task
import StarPU.Data
import StarPU.AccessMode

import Data.Word
import Data.Ix
import Control.Monad
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.IO.Unsafe
import Data.List

foreign import ccall "starpu_data_unregister" dataUnregister :: Handle -> IO ()
foreign import ccall "starpu_data_unregister_no_coherency" dataUnregisterInvalid :: Handle -> IO ()
foreign import ccall "starpu_data_invalidate" dataInvalidate :: Handle -> IO ()
foreign import ccall "starpu_data_release" dataRelease :: Handle -> IO ()

foreign import ccall "starpu_data_acquire" dataAcquire :: Handle -> AccessMode -> IO Int

unregister :: Data a => a -> IO ()
unregister a = dataUnregister (handle a)

unregisterInvalid :: Data a => a -> IO ()
unregisterInvalid a = dataUnregisterInvalid (handle a)

invalidate :: Data a => a -> IO ()
invalidate a = dataInvalidate (handle a)

release :: Data a => a -> IO ()
release a = dataRelease (handle a)

acquire :: Data a => AccessMode -> a -> IO Int
acquire mode a = dataAcquire (handle a) mode

foreign import ccall unsafe "starpu_malloc_ex" starpuMalloc :: CSize -> IO (Ptr ())
