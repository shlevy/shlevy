{-# LANGUAGE QuantifiedConstraints #-}

module CapIO
  ( CapIO
  , runCapIO
  , runCapIOST
  , forgeRootIO
  , RootCap
  , HasRootCap
  , HasIOCap
  , Root
  , ValidState (..)
  , Capability (..)
  , CapabilityData
  , HasCapability
  , withCapability
  , HasPureCap
  , PureCap
  , Pure
  , runCapIOPure
  )
where

import CapIO.Core
import CapIO.Prelim
import Control.Monad.ST
import Data.Coerce

type HasIOCap = HasRootCap RealWorld

runCapIO :: CapIO RealWorld a -> (HasIOCap) => IO a
runCapIO go = sudo RealWorld (coerce go)

runCapIOST :: forall s a. (ValidState s) => CapIO s a -> (HasRootCap s) => ST s a
runCapIOST go = sudo s (coerce go)

data Pure = MkPure

type PureCap = MkCapability Pure

type HasPureCap s = HasCapability s PureCap

runCapIOPure
  :: forall a
   . (forall s. ValidState s)
  => (forall s. (HasRootCap s, HasPureCap s) => CapIO s a)
  -> a
runCapIOPure go = runST go'
 where
  go' :: forall s. ST s a
  go' = do
    let pc = createCapabilityData @s MkPure
    root <- forgeRootST
    withCapability root $ withCapability pc $ runCapIOST go
