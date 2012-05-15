module StarPU.Event where

import Foreign.C

type Event = CUInt

dummyEvent :: Event
dummyEvent = 0
