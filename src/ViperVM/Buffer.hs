module ViperVM.Buffer where

import ViperVM.Memory

-- |
-- A buffer is a set of contiguous memory cells
--
data Buffer = Buffer {
  memory :: Memory,     -- ^ Memory containing the buffer
  size :: Integer,      -- ^ Size of the buffer in bytes
  id :: Int             -- ^ Identifier of this buffer (for the memory manager)
}
