module CapIO
  ( module CapIO.Core
  , runCapIO
  , liftIO
  , unliftIO
  , module CapIO.Capability
  )
where

import CapIO.Capability
import CapIO.Core
import Data.Coerce

runCapIO :: ((HasIOCap) => CapIO a) -> IO a
runCapIO go = do
  ioc <- forgeIOCap
  withCapability ioc $ sudo (coerce go)

liftIO :: (HasIOCap) => IO a -> CapIO a
liftIO go = sudo (coerce go)

unliftIO :: (HasIOCap) => ((forall a. CapIO a -> IO a) -> IO b) -> CapIO b
unliftIO useRunInIO = sudo (coerce (useRunInIO coerce))
