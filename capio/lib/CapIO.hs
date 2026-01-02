{-# LANGUAGE QuantifiedConstraints #-}

module CapIO
  ( CapIO
  , runCapIO
  , runCapIOST
  , forgeRootIO
  , ValidState (..)
  , Capability (..)
  , CapabilityData
  , HasCapability
  , withCapability
  , runCapIOPure
  )
where

import CapIO.Core
import CapIO.Prelim
import Control.Monad.ST
import Data.Coerce

runCapIO :: CapIO RealWorld a -> (HasCapability RealWorld Root) => IO a
runCapIO go = sudo RealWorld (coerce go)

runCapIOST :: forall s a. (ValidState s) => CapIO s a -> (HasCapability s Root) => ST s a
runCapIOST go = sudo s (coerce go)

runCapIOPure
  :: forall a
   . (forall s. ValidState s)
  => (forall s. (HasCapability s Root) => CapIO s a)
  -> a
runCapIOPure go = runST go'
 where
  go' :: forall s. ST s a
  go' = do
    root <- forgeRootST
    withCapability root $ runCapIOST go
