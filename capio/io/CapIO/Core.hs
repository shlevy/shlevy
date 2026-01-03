{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module CapIO.Core
  ( CapIO
  , CapabilityData
  , HasCapability
  , withCapability
  , capabilityData
  , RootCap
  , HasRootCap
  , Root
  , ValidState (..)
  , forgeRootIO
  )
where

import CapIO.Prelim
import CapIO.Trustworthy
import Control.Monad.Fix
import Control.Monad.ST
import Data.Coerce
import Data.Kind

type family CapIO (s :: Type) = (m :: Type -> Type) | m -> s where
  CapIO RealWorld = IO

type role CapabilityData nominal nominal

type CapabilityData :: Type -> Capability -> Type
data CapabilityData s cap where
  MkCapabilityData :: CapabilityData RealWorld (MkCapability authority)

type HasCapability' :: Capability -> Constraint
type family HasCapability' cap where
  HasCapability' (MkCapability authority) = ()

type HasCapability :: Type -> Capability -> Constraint
type family HasCapability s cap where
  HasCapability s cap = (HasCapability' cap, s ~ RealWorld, HasCapabilityData cap)

class HasCapabilityData (cap :: Capability) where
  capabilityData' :: (HasCapability' cap) => CapabilityData RealWorld cap

instance HasCapabilityData (MkCapability authority) where
  capabilityData' = MkCapabilityData

withCapability :: CapabilityData s cap -> ((HasCapability s cap) => x) -> x
withCapability MkCapabilityData go = go

capabilityData :: (HasCapability s cap) => CapabilityData s cap
capabilityData = capabilityData'

type RootCap = MkCapability Root

type HasRootCap s = HasCapability s RootCap

data Root

class (MonadFix (CapIO s)) => ValidState s where
  forgeRootST :: ST s (CapabilityData s RootCap)
  sudo
    :: forall s'
    ->(m ~ CapIO s, s ~ s')
    => ( ( forall a b. (Coercible a b) => Coercible (m a) (ST s b)
         , forall a b. (s ~ RealWorld, Coercible a b) => Coercible (m a) (IO b)
         )
         => x
       )
    -> (HasRootCap s) => x
  createCapabilityData :: authority -> CapabilityData s (MkCapability authority)

instance (STRealWorld) => ValidState RealWorld where
  forgeRootST = pure MkCapabilityData
  sudo _ go = go
  createCapabilityData _ = MkCapabilityData

forgeRootIO :: IO (CapabilityData RealWorld RootCap)
forgeRootIO = pure MkCapabilityData
