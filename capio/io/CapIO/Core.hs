{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module CapIO.Core
  ( CapIO
  , CapabilityData
  , forgeRootIO
  , HasCapability
  , withCapability
  , capabilityData
  , ValidState (..)
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

type CapabilityData' :: Capability -> Type
type family CapabilityData' cap where
  CapabilityData' Root = ()

type role CapabilityData nominal nominal

type CapabilityData :: Type -> Capability -> Type
data CapabilityData s cap where
  MkCapabilityData :: (WithCapability cap) => CapabilityData' cap -> CapabilityData RealWorld cap

type HasCapability' :: Capability -> Constraint
type family HasCapability' cap where
  HasCapability' Root = ()

type HasCapability :: Type -> Capability -> Constraint
type family HasCapability s cap where
  HasCapability s cap = (HasCapability' cap, s ~ RealWorld, HasCapabilityData cap)

class HasCapabilityData (cap :: Capability) where
  capabilityData' :: (HasCapability' cap) => CapabilityData RealWorld cap

instance HasCapabilityData Root where
  capabilityData' = MkCapabilityData ()

class WithCapability (cap :: Capability) where
  withCapability' :: CapabilityData' cap -> ((HasCapability RealWorld cap) => x) -> x

instance WithCapability Root where
  withCapability' _ go = go

withCapability :: CapabilityData s cap -> ((HasCapability s cap) => x) -> x
withCapability (MkCapabilityData cd) = withCapability' cd

capabilityData :: (HasCapability s cap) => CapabilityData s cap
capabilityData = capabilityData'

class (MonadFix (CapIO s)) => ValidState s where
  forgeRootST :: ST s (CapabilityData s Root)
  sudo
    :: forall s'
    ->(m ~ CapIO s, s ~ s')
    => ( ( forall a b. (Coercible a b) => Coercible (m a) (ST s b)
         , forall a b. (s ~ RealWorld, Coercible a b) => Coercible (m a) (IO b)
         )
         => x
       )
    -> (HasCapability s Root) => x

instance (STRealWorld) => ValidState RealWorld where
  forgeRootST = pure $ MkCapabilityData ()
  sudo _ go = go

forgeRootIO :: IO (CapabilityData RealWorld Root)
forgeRootIO = pure $ MkCapabilityData ()
