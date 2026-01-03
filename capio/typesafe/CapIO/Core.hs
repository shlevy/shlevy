{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module CapIO.Core
  ( CapIO'
  , CapIO
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
import GHC.TypeLits

newtype CapIO' s a = MkCapIO (ST s a) deriving newtype (Functor, Applicative, Monad, MonadFix)

type family CapIO (s :: Type) = (m :: Type -> Type) | m -> s where
  CapIO s = CapIO' s

type role CapabilityData nominal nominal

type CapabilityData :: Type -> Capability -> Type
data CapabilityData s cap where
  RootCap :: CapabilityData s Root

type RootKey = NewSym CapabilityData

type CapKey :: Capability -> Symbol
type family CapKey cap where
  CapKey Root = RootKey

type HasCapability :: Type -> Capability -> Constraint
type family HasCapability s cap where
  HasCapability s cap = IP (CapKey cap) (CapabilityData s cap)

withCapability :: CapabilityData s cap -> ((HasCapability s cap) => x) -> x
withCapability RootCap = bindImplicit RootCap

capabilityData :: forall s cap. (HasCapability s cap) => CapabilityData s cap
capabilityData = ip @(CapKey cap)

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

instance (STRealWorld) => ValidState s where
  forgeRootST = pure RootCap
  sudo _ go = go

forgeRootIO :: IO (CapabilityData RealWorld Root)
forgeRootIO = pure RootCap
