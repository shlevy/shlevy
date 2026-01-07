module CapIO
  ( module CapIO.Core
  , runCapIO
  , module CapIO.Capability
  )
where

import CapIO.Capability
import CapIO.Core
import Data.Coerce

runCapIO :: CapIO a -> IO a
runCapIO go = do
  ioc <- forgeIOCap
  withCapability ioc $ sudo (coerce go)
