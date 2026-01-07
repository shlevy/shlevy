{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CapIO.Capability.Core where

import CapIO.Capability.Types hiding (CapabilityKey)
import CapIO.Capability.Types qualified as Types
import CapIO.Trustworthy
import Control.Exception.Context
import Data.Coerce
import Data.Kind
import Data.List (List)
import Data.Tagged
import Data.Type.Bool
import GHC.TypeLits

type WrapperT :: (Type -> Type) -> (Type -> Type)
type WrapperT = Tagged

class (forall a wrapper. (Coercible a (wrapper a)) => Coercible a (WrapperT wrapper a)) => ValidWrapperT

instance ValidWrapperT

type NeedsData :: forall k. k -> Bool
type family NeedsData x where
  NeedsData (cap :: Type -> Type) = NeedsData (Data cap) || NeedsData (Attenuations cap)
  NeedsData (Just (t :: Type)) = True
  NeedsData (Nothing :: Maybe Type) = False
  NeedsData ('[] :: List (Type -> Type)) = False
  NeedsData ((hd :: (Type -> Type)) : tl) = NeedsData hd || NeedsData tl

data SBool :: Bool -> Type where
  STrue :: SBool True
  SFalse :: SBool False

needsData :: forall cap -> (Capability cap) => SBool (NeedsData cap)
needsData cap = case knownData (Data cap) of
  SJust -> STrue
  SNothing -> needsDataCaps (Attenuations cap)

needsDataCaps :: forall caps -> (KnownCaps caps) => SBool (NeedsData caps)
needsDataCaps caps = case knownCaps caps of
  SNil -> SFalse
  SCons @hd @tl -> case needsData hd of
    STrue -> STrue
    SFalse -> needsDataCaps tl

type CapabilityKey :: (Type -> Type) -> CapName -> Symbol
type family CapabilityKey cap key where
  CapabilityKey cap key =
    If
      (NeedsData cap) -- Then
      (Types.CapabilityKey cap key) -- Else
      "exceptionContext"

type family CapabilityTy (cap :: Type -> Type) :: Type where
  CapabilityTy cap =
    If
      (NeedsData cap) -- Then
      (CapabilityHandle WrapperT cap) -- Else
      ExceptionContext

type HasNamedCapability name cap = (IP (CapabilityKey cap name) (CapabilityTy cap), Capability cap)

withNamedCapability :: forall name -> (Capability cap) => CapabilityHandle WrapperT cap -> ((HasNamedCapability name cap) => x) -> x
withNamedCapability @cap name ch go = case needsData cap of
  STrue -> bindImplicit (CapabilityKey cap name) ch go
  SFalse -> go

conjureCapabilitiesHandle :: (KnownCaps caps, NeedsData caps ~ False) => CapabilitiesHandle WrapperT caps
conjureCapabilitiesHandle @caps = case knownCaps caps of
  SNil -> NilCsH
  SCons @hd @tl -> case needsData hd of
    SFalse -> case needsDataCaps tl of
      SFalse -> conjureCapabilityHandle ::: conjureCapabilitiesHandle

conjureCapabilityHandle :: (Capability cap, NeedsData cap ~ False) => CapabilityHandle WrapperT cap
conjureCapabilityHandle @cap = case knownData (Data cap) of
  SNothing -> Tagged (MkCapabilityHandle NoData conjureCapabilitiesHandle)

namedCapabilityHandle :: forall name -> (HasNamedCapability name cap) => CapabilityHandle WrapperT cap
namedCapabilityHandle @cap name = case needsData cap of
  STrue -> ip @(CapabilityKey cap name)
  SFalse -> conjureCapabilityHandle

type family Drop (t :: Type) :: Type where
  Drop t = t

dropKeyed :: forall k -> (IP k t) => ((IP k (Drop t)) => x) -> x
dropKeyed _ go = go
