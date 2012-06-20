module ViperVM.Runtime
	where

import ViperVM.Memory
import ViperVM.Driver
import Control.Monad.State

type Runtime = State RuntimeState

data RuntimeState = RS {
	mems :: [Memory]
}

memories :: Runtime [Memory]
memories = do
	r <- get
	return []


init :: Driver a => [a] -> Runtime ()
init ds = do
	let mems = map memos ds
	put $ RS mems
	return ()
