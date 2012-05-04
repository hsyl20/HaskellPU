module Task where

import Event

import Foreign.Ptr
import Foreign.C

type Task = Ptr ()

foreign import ccall unsafe "starpu_task_create_ex" taskCreate :: IO Task
foreign import ccall unsafe "starpu_task_destroy" taskDestroy :: Task -> IO ()
foreign import ccall unsafe "starpu_task_submit" taskSubmit :: Task -> IO CInt
foreign import ccall "starpu_task_wait" taskWait :: Task -> IO ()
foreign import ccall "starpu_task_wait_for_all" taskWaitForAll :: IO ()


foreign import ccall unsafe "starpu_task_tag_get" taskEvent :: Task -> Event
foreign import ccall unsafe "starpu_task_depends_on" taskDependsOn :: Task -> Event -> IO ()
