module HaskellPU.Driver
	where

import HaskellPU.Memory

class Driver a where
	memos :: a -> [Memory]
