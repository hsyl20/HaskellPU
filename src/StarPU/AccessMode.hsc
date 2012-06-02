module StarPU.AccessMode where

import Foreign.C

#include <starpu.h>

newtype AccessMode = AccessMode { mode :: CInt }
#{enum AccessMode, AccessMode,
  none = STARPU_NONE,
  readOnly = STARPU_R,
  writeOnly = STARPU_W,
  readWrite = STARPU_RW,
  scratch = STARPU_SCRATCH,
  redux = STARPU_REDUX
}
