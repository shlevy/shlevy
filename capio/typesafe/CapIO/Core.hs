{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module CapIO.Core
  ( CapIO
  , CapabilityData
  , RestoreCapabilityData
  , AuthorityKey
  , AuthorityData
  , HasCapability
  , withCapability
  , capabilityData
  , RootCap
  , HasRootCap
  , Root
  , ValidState (..)
  , forgeRootIO
  , CapIO'
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
  MkCapabilityData :: authority -> CapabilityData s (MkCapability authority)

class RestoreCapabilityData (s :: Type) (cap :: Capability)

instance RestoreCapabilityData (s :: Type) (cap :: Capability)

type family AuthorityKey (cap :: Capability) :: Symbol where
  AuthorityKey (MkCapability authority) = NewSym authority

type family AuthorityData (s :: Type) (cap :: Capability) :: Type where
  AuthorityData s cap = CapabilityData s cap

type HasCapability s cap = (IP (AuthorityKey cap) (AuthorityData s cap), RestoreCapabilityData s cap)

withCapability :: forall s cap x. CapabilityData s cap -> ((HasCapability s cap) => x) -> x
withCapability cd go = bindImplicit (AuthorityKey cap) cd go {- HLINT ignore "Eta reduce" -}

capabilityData :: forall s cap. (HasCapability s cap) => CapabilityData s cap
capabilityData = ip @(AuthorityKey cap)

type RootCap = MkCapability Root

type HasRootCap s = HasCapability s RootCap

data Root = MkRoot

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

instance (STRealWorld) => ValidState s where
  forgeRootST = pure $ MkCapabilityData MkRoot
  sudo _ go = go
  createCapabilityData = MkCapabilityData

forgeRootIO :: IO (CapabilityData RealWorld RootCap)
forgeRootIO = pure $ MkCapabilityData MkRoot
