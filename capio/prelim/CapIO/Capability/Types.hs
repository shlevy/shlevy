{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}

module CapIO.Capability.Types
  ( Capability (..)
  , CapabilityHandle
  , CapabilityHandle' (..)
  , CapData (..)
  , CapabilitiesHandle (..)
  , KnownData (..)
  , SMaybe (..)
  , KnownCaps (..)
  , SCaps (..)
  , CapabilityKey
  , CapName (..)
  )
where

import Data.Kind
import Data.List (List)
import Data.Typeable
import Data.VarFun
import GHC.TypeLits

class (KnownData (Data cap), KnownCaps (Attenuations cap)) => Capability (cap :: Type -> Type) where
  type Data cap :: Maybe Type
  type Data cap = Nothing
  type Attenuations cap :: List (Type -> Type)
  type Attenuations cap = '[]

data CapabilityHandle' wrapperT :: (Type -> Type) -> Type where
  MkCapabilityHandle :: CapData (Data cap) -> CapabilitiesHandle wrapperT (Attenuations cap) -> CapabilityHandle' wrapperT cap

type CapabilityHandle wrapperT cap = wrapperT cap (CapabilityHandle' wrapperT cap)

data CapData :: Maybe Type -> Type where
  NoData :: CapData Nothing
  MkData :: t -> CapData (Just t)

data SMaybe :: Maybe Type -> Type where
  SNothing :: SMaybe Nothing
  SJust :: (Typeable t) => SMaybe (Just t)

instance VarFun (CapData Nothing) where
  type FunArgs (CapData Nothing) = '[]
  liftF useCD = useCD NoData

instance VarFun (CapData (Just t)) where
  type FunArgs (CapData (Just t)) = '[t]
  liftF useCD = useCD . MkData

class (VarFun (CapData mdata)) => KnownData mdata where
  knownData :: forall mdata' -> (mdata ~ mdata') => SMaybe mdata

instance KnownData Nothing where
  knownData _ = SNothing

instance (Typeable t) => KnownData (Just t) where
  knownData _ = SJust

data SCaps :: List (Type -> Type) -> Type where
  SNil :: SCaps '[]
  SCons :: (Typeable hd, Capability hd, KnownCaps tl) => SCaps (hd : tl)

class (forall wrapperT. VarFun (CapabilitiesHandle wrapperT caps)) => KnownCaps caps where
  knownCaps :: forall caps' -> (caps ~ caps') => SCaps caps

instance KnownCaps '[] where
  knownCaps _ = SNil

instance (Capability hd, Typeable hd, KnownCaps tl) => KnownCaps (hd : tl) where
  knownCaps _ = SCons

data CapabilitiesHandle wrapperT :: List (Type -> Type) -> Type where
  NilCsH :: CapabilitiesHandle wrapperT '[]
  (:::) :: CapabilityHandle wrapperT hd -> CapabilitiesHandle wrapperT tl -> CapabilitiesHandle wrapperT (hd : tl)

instance VarFun (CapabilitiesHandle wrapperT '[]) where
  type FunArgs (CapabilitiesHandle wrapperT '[]) = '[]
  liftF useCsH = useCsH NilCsH

instance (VarFun (CapabilitiesHandle wrapperT tl)) => VarFun (CapabilitiesHandle wrapperT (hd : tl)) where
  type FunArgs (CapabilitiesHandle wrapperT (hd : tl)) = CapabilityHandle wrapperT hd : FunArgs (CapabilitiesHandle wrapperT tl)
  liftF useCsH ch = liftF $ useCsH . (ch :::)

type CapabilityKey' :: forall sym. (Type -> Type) -> CapName -> sym
data family CapabilityKey' cap name :: sym

data CapName = Default | forall k. Named k

type CapabilityKey :: (Type -> Type) -> CapName -> Symbol
type CapabilityKey = CapabilityKey'
