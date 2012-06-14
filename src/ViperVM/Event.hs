module ViperVM.Event where

import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe

type Event = Ptr ()

foreign import ccall "starpu_event_create" eventCreate :: IO Event
foreign import ccall "starpu_event_destroy" eventDestroy :: Event -> IO ()
foreign import ccall "starpu_event_wait" eventWait :: Event -> IO ()
foreign import ccall "starpu_event_trigger" eventTrigger :: Event -> IO ()
foreign import ccall "starpu_event_dummy" dummyEvent :: Event
