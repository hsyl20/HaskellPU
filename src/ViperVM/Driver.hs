module ViperVM.Driver
	where

import ViperVM.Memory

class Driver a where
	memos :: a -> [Memory]
