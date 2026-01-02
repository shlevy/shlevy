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
import Control.Monad.Fix
import Control.Monad.ST
import Data.Coerce
import Data.Kind
import Data.Singletons
import GHC.IO (IO (..))
import GHC.ST (ST (..))

type family CapIO (s :: Type) = (m :: Type -> Type) | m -> s where
  CapIO RealWorld = IO

type role CapabilityData nominal nominal

type CapabilityData :: Type -> Capability -> Type
data CapabilityData s cap where
  RootCap :: CapabilityData RealWorld Root

type HasCapability' :: Capability -> Constraint
type family HasCapability' cap where
  HasCapability' Root = ()

type HasCapability :: Type -> Capability -> Constraint
type family HasCapability s cap where
  HasCapability s cap = (HasCapability' cap, s ~ RealWorld, SingI cap)

withCapability :: CapabilityData s cap -> ((HasCapability s cap) => x) -> x
withCapability RootCap go = go

capabilityData :: forall s cap. (HasCapability s cap) => CapabilityData s cap
capabilityData = case sing @cap of
  SRoot -> RootCap

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

instance ValidState RealWorld where
  forgeRootST = pure RootCap
  sudo _ go = go

forgeRootIO :: IO (CapabilityData RealWorld Root)
forgeRootIO = pure RootCap
