{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}

module CapIO.Capability
  ( Capability (..)
  , CapabilityHandle
  , CapabilityHandle'
  , pattern MkCapabilityHandle
  , CapData (..)
  , CapabilitiesHandle
  , pattern NilCsH
  , pattern (:::)
  , pattern (:!)
  , pattern (:!:)
  , CanForge
  , forgeHandle
  , capabilityHandleAttenuations
  , capabilityHandleData
  , CapName (..)
  , HasNamedCapability
  , HasCapability
  , withNamedCapability
  , withCapability
  , namedCapabilityHandle
  , capabilityHandle
  , LacksNamedCapability
  , dropNamedCapability
  , LacksCapability
  , dropCapability
  , renameCapability
  , namedCapabilityData
  , capabilityData
  , NamesCaps
  , HasNamedCapabilities
  , withNamedCapabilities'
  , withNamedCapabilitiesHandle
  , namedCapsHandle
  , withNamedCapabilities
  , withCapabilities
  , withCapabilitiesHandle
  , HasCapabilities
  , unattenuateNamedWithNames
  , unattenuateWithNames
  , unattenuateNamed
  , unattenuate
  , GoodCapabilities
  , Defaults
  , forgeNamingAttenuations
  , forge
  )
where

import CapIO.Capability.Core
import CapIO.Capability.Types (CapData (..), CapName (..), Capability (..), pattern MkCapabilityHandle, pattern NilCsH, pattern (:::))
import CapIO.Capability.Types qualified as Types
import CapIO.Trustworthy
import Data.Coerce
import Data.Kind
import Data.List (List)
import Data.VarFun

type CapabilityHandle cap = Types.CapabilityHandle WrapperT cap

type CapabilitiesHandle = Types.CapabilitiesHandle WrapperT

type CapabilityHandle' = Types.CapabilityHandle' WrapperT

pattern (:!) :: CapabilityHandle hd -> CapabilitiesHandle '[hd]
pattern (:!) hd = (hd ::: NilCsH)

pattern (:!:) :: CapabilityHandle penult -> CapabilityHandle ult -> CapabilitiesHandle '[penult, ult]
pattern (:!:) x y = x ::: (:!) y

type CanForge cap = Coercible (Types.CapabilityHandle' WrapperT cap) (cap (Types.CapabilityHandle' WrapperT cap))

forgeHandle :: forall cap -> (Capability cap, CanForge cap) => CapData (Data cap) ~> CapabilitiesHandle (Attenuations cap) ~> CapabilityHandle cap
forgeHandle cap = liftF $ liftF . mkCH
 where
  mkCH :: (ValidWrapperT) => CapData (Data cap) -> CapabilitiesHandle (Attenuations cap) -> CapabilityHandle cap
  mkCH = coerce (MkCapabilityHandle @cap)

capabilityHandleAttenuations :: forall cap. (CanForge cap) => CapabilityHandle cap -> CapabilitiesHandle (Attenuations cap)
capabilityHandleAttenuations cap = case cap' of
  Types.MkCapabilityHandle _ csv -> csv
 where
  cap' :: (ValidWrapperT) => Types.CapabilityHandle' WrapperT cap
  cap' = coerce cap

capabilityHandleData :: forall cap t. (CanForge cap, Data cap ~ Just t) => CapabilityHandle cap -> t
capabilityHandleData cap = case cap' of
  (Types.MkCapabilityHandle (Types.MkData v) _) -> v
 where
  cap' :: (ValidWrapperT) => Types.CapabilityHandle' WrapperT cap
  cap' = coerce cap

type HasCapability cap = HasNamedCapability Default cap

withCapability :: (Capability cap) => CapabilityHandle cap -> ((HasCapability cap) => x) -> x
withCapability = withNamedCapability Default

capabilityHandle :: (HasCapability cap) => CapabilityHandle cap
capabilityHandle = namedCapabilityHandle Default

type LacksNamedCapability name cap = IP (CapabilityKey cap name) (Drop (CapabilityTy cap))

dropNamedCapability :: forall name cap -> (HasNamedCapability name cap) => ((LacksNamedCapability name cap) => x) -> x
dropNamedCapability name cap = dropKeyed (CapabilityKey cap name)

type LacksCapability cap = LacksNamedCapability Default cap

dropCapability :: forall cap -> (HasCapability cap) => ((LacksCapability cap) => x) -> x
dropCapability = dropNamedCapability Default

renameCapability :: forall cap from to -> (HasNamedCapability from cap) => ((HasNamedCapability to cap) => x) -> x
renameCapability cap from to go = bindImplicit (CapabilityKey cap to) (ip @(CapabilityKey cap from)) go {- HLINT ignore "Eta reduce" -}

namedCapabilityData :: forall name cap -> (HasNamedCapability name cap, Data cap ~ Just t, CanForge cap) => t
namedCapabilityData name cap = capabilityHandleData (namedCapabilityHandle @cap name)

capabilityData :: forall cap -> (HasCapability cap, Data cap ~ Just t, CanForge cap) => t
capabilityData = namedCapabilityData Default

class NamesCaps (names :: List CapName) (caps :: List (Type -> Type)) where
  type HasNamedCapabilities names caps :: Constraint
  withNamedCapabilities' :: forall names' caps' x -> (names ~ names', caps ~ caps') => ((HasNamedCapabilities names caps) => x) -> CapabilitiesHandle caps ~> x
  withNamedCapabilitiesHandle :: forall names' -> (names ~ names') => CapabilitiesHandle caps -> ((HasNamedCapabilities names caps) => x) -> x
  namedCapsHandle :: forall names' caps' -> (names ~ names', caps ~ caps') => (CapabilitiesHandle caps ~> ret) -> ((HasNamedCapabilities names caps) => ret)

instance NamesCaps '[] '[] where
  type HasNamedCapabilities '[] '[] = ()
  withNamedCapabilities' _ _ _ go = go
  withNamedCapabilitiesHandle _ _ go = go
  namedCapsHandle _ _ go = go

instance (Capability cpHd, NamesCaps nmTl cpTl) => NamesCaps (nmHd : nmTl) (cpHd : cpTl) where
  type HasNamedCapabilities (nmHd : nmTl) (cpHd : cpTl) = (HasNamedCapability nmHd cpHd, HasNamedCapabilities nmTl cpTl)
  withNamedCapabilities' _ _ x go ch = withNamedCapability nmHd ch (withNamedCapabilities' nmTl cpTl x go)
  withNamedCapabilitiesHandle _ (cshHd ::: cshTl) go = withNamedCapability nmHd cshHd (withNamedCapabilitiesHandle nmTl cshTl go)
  namedCapsHandle _ _ go = namedCapsHandle nmTl cpTl (go (namedCapabilityHandle nmHd))

-- Work around limitation of RequiredTypeArguments
type HNCX names caps x = ((HasNamedCapabilities names caps) => x) -> x

withNamedCapabilities :: forall names caps -> (NamesCaps names caps) => CapabilitiesHandle caps ~> (((HasNamedCapabilities names caps) => x) -> x)
withNamedCapabilities @x names caps = withNamedCapabilities' names caps (HNCX names caps x) (\x -> x {- HLINT ignore "Use id" -})

type HasCapabilities caps = HasNamedCapabilities (Defaults caps) caps

withCapabilities :: forall caps -> (GoodCapabilities caps) => CapabilitiesHandle caps ~> (((HasCapabilities caps) => x) -> x)
withCapabilities @x caps = withNamedCapabilities @x (Defaults caps) caps

unattenuateNamedWithNames :: forall name cap names -> (HasNamedCapability name cap, CanForge cap, NamesCaps names (Attenuations cap)) => ((HasNamedCapabilities names (Attenuations cap)) => x) -> x
unattenuateNamedWithNames name cap names = withNamedCapabilitiesHandle names (capabilityHandleAttenuations (namedCapabilityHandle @cap name))

unattenuateWithNames :: forall cap names -> (HasCapability cap, CanForge cap, NamesCaps names (Attenuations cap)) => ((HasNamedCapabilities names (Attenuations cap)) => x) -> x
unattenuateWithNames = unattenuateNamedWithNames Default

unattenuateNamed :: forall name cap -> (HasNamedCapability name cap, CanForge cap, GoodCapabilities (Attenuations cap)) => ((HasCapabilities (Attenuations cap)) => x) -> x
unattenuateNamed name cap = unattenuateNamedWithNames name cap (Defaults (Attenuations cap))

unattenuate :: forall cap -> (HasCapability cap, CanForge cap, GoodCapabilities (Attenuations cap)) => ((HasCapabilities (Attenuations cap)) => x) -> x
unattenuate = unattenuateNamed Default

class (NamesCaps (Defaults caps) caps) => GoodCapabilities caps where
  type Defaults caps :: List CapName

instance GoodCapabilities '[] where
  type Defaults '[] = '[]

instance (Capability hd, GoodCapabilities tl) => GoodCapabilities (hd : tl) where
  type Defaults (hd : tl) = Default : Defaults tl

withCapabilitiesHandle :: (GoodCapabilities caps) => CapabilitiesHandle caps -> ((HasCapabilities caps) => x) -> x
withCapabilitiesHandle @caps = withNamedCapabilitiesHandle (Defaults caps)

forgeNamingAttenuations :: forall names cap -> (CanForge cap, Capability cap, NamesCaps names (Attenuations cap), HasNamedCapabilities names (Attenuations cap)) => CapData (Data cap) ~> CapabilityHandle cap
forgeNamingAttenuations names cap = fmapVariable (CapData (Data cap)) nch (forgeHandle cap)
 where
  nch :: (CapabilitiesHandle (Attenuations cap) ~> CapabilityHandle cap) -> CapabilityHandle cap
  nch f = namedCapsHandle names (Attenuations cap) f {- HLINT ignore "Eta reduce" -}

forge :: forall cap -> (CanForge cap, Capability cap, GoodCapabilities (Attenuations cap), HasCapabilities (Attenuations cap)) => CapData (Data cap) ~> CapabilityHandle cap
forge cap = forgeNamingAttenuations (Defaults (Attenuations cap)) cap
