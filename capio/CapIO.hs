module CapIO where

import System.IO.Unsafe

type CapIO = IO

data IOCap = MkIOCap

data PureCap = MkPureCap

purity :: IOCap -> PureCap
purity _ = MkPureCap

runCapIO :: (IOCap -> CapIO a) -> IO a
runCapIO go = go MkIOCap

runPureIO :: (PureCap -> CapIO a) -> a
runPureIO go = unsafePerformIO (go MkPureCap)
