{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}

module CapIO.Capability.Core where

import CapIO.Capability.Types hiding (CapabilityKey)
import CapIO.Capability.Types qualified as Types
import CapIO.Trustworthy
import Control.Monad.Trans.Identity
import Data.Coerce
import Data.Kind
import Data.Proxy
import GHC.TypeLits

type WrapperT :: (Type -> Type) -> (Type -> Type)
type WrapperT = IdentityT

class (forall a wrapper. (Coercible a (wrapper a)) => Coercible a (WrapperT wrapper a)) => ValidWrapperT

instance ValidWrapperT

type CapabilityKey :: (Type -> Type) -> CapName -> Symbol
type family CapabilityKey cap key where
  CapabilityKey cap key = Types.CapabilityKey cap key

type family CapabilityTy (cap :: Type -> Type) :: Type where
  CapabilityTy cap = CapabilityHandle WrapperT cap

type HasNamedCapability (name :: CapName) cap = (IP (CapabilityKey cap name) (CapabilityTy cap), Capability cap)

withNamedCapability :: forall name -> (Capability cap) => CapabilityHandle WrapperT cap -> ((HasNamedCapability name cap) => x) -> x
withNamedCapability @cap name ch go = bindImplicit (CapabilityKey cap name) ch go {- HLINT ignore "Eta reduce" -}

namedCapabilityHandle :: forall name -> (HasNamedCapability name cap) => CapabilityHandle WrapperT cap
namedCapabilityHandle @cap name = ip @(CapabilityKey cap name)

type family Drop (t :: Type) :: Type where
  Drop t = Proxy t

dropKeyed :: forall k -> (IP k t) => ((IP k (Drop t)) => x) -> x
dropKeyed k = bindImplicit k Proxy
