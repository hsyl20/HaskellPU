module StarPU.Data where

import Foreign.Ptr
import StarPU.Event

type Handle = Ptr ()

class Data a where
  handle :: a -> Handle
  event :: a -> Event
