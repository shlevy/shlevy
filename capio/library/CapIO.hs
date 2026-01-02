{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
module CapIO
{-
  ( Core.CapIO
  , Core.ValidState
  , -- IO
    CIO
--  , runCapIO
  , RealWorld
  , -- Pure
    CanRunPure
--  , runPure
  , -- Capabilities
    Core.AttenuateKey
  , Core.AttenuateData
  , Core.Drop
  )
-}
where

import CapIO.Core qualified as Core
import CapIO.Core (CapIO, ValidState)
import CapIO.Prelim
import Control.Monad.ST
import GHC.Base (IP)
import Data.Kind
import Data.Coerce
import GHC.TypeLits
import GHC.Exts (TYPE, LiftedRep, RuntimeRep(..))
import Data.List (List)
-- example, remove
import Data.STRef

data AmbientCap

-- Allow new instances? Maybe just a closed type with relevant families
instance Capability AmbientCap where
  type CapabilityParams AmbientCap = Core.AmbientCapParams

type HasAmbientCap s = HasCap AmbientCap s
type HasIOCap = HasAmbientCap RealWorld
type CIO = CapIO RealWorld

runCapIO :: (HasIOCap => CIO a) -> IO a
runCapIO go = Core.withAmbientCapParams RealWorld $ Core.withCoercibleST RealWorld $ coerce go

liftIO :: HasIOCap => IO a -> CIO a
liftIO = Core.withCoercibleST RealWorld coerce

unliftIO :: HasIOCap => ((forall a . CIO a -> IO a) -> IO b) -> CIO b
unliftIO go = Core.withCoercibleST RealWorld (coerce (go coerce))

runCapIOST :: forall s a. ValidState s => (HasAmbientCap s => CapIO s a) -> ST s a
runCapIOST go = Core.withAmbientCapParams s $ Core.withCoercibleST s $ coerce go

liftST :: forall s a. HasAmbientCap s => ST s a -> CapIO s a
liftST = Core.withCoercibleST s coerce

unliftST :: forall s b. HasAmbientCap s => ((forall a . CapIO s a -> ST s a) -> ST s b) -> CapIO s b
unliftST go = Core.withCoercibleST s (coerce (go coerce))

type role PureCap nominal nominal
type PureCap :: (Type -> Type) -> Type -> Type
newtype PureCap cap s = MkPureCap (cap s)
deriving via (Attenuate AmbientCap PureCap) instance Capability PureCap

type CanRunPure = (forall s. ValidState s)

runPureCapIO :: forall a . CanRunPure => (forall s. (HasCaps (PureCap ::! AmbientCap) s) => CapIO s a) -> a
runPureCapIO go = runST go'
  where
  go' :: forall s. ST s a
  go' = runCapIOST (Core.attenuate (CapabilityParams AmbientCap) PureCap s go)

rescopeCaps :: forall s a. forall caps -> HasCaps caps s => (forall s'. HasCaps caps s' => CapIO s' a)  -> CapIO s a
rescopeCaps _ go = go @s

type HasCap :: forall k. k -> Type -> Constraint
type HasCap cap s = HasCaps (Sing cap) s

type family HasCaps (caps :: [SomeCap]) (s :: Type) :: Constraint where
  HasCaps '[] s = (ValidState s)
  HasCaps (MkSomeCap c : tl) s = (WithParams (CapabilityParams c) s, HasCaps tl s)

data SomeCap = forall k. MkSomeCap k
type Sing :: forall k. k -> [SomeCap]
type Sing c = '[MkSomeCap c]
type (::!) :: forall k1 k2. k1 -> k2 -> [SomeCap]
type (::!) c1 c2 = [MkSomeCap c1, MkSomeCap c2]
type (:::) :: forall k. k -> [SomeCap] -> [SomeCap]
type (:::) c tl = MkSomeCap c : tl
infixr 5 :::
infixr 5 ::!

type Capability :: forall k. k -> Constraint
class Capability cap where
  type CapabilityParams cap :: List Param

type Attenuate :: forall k. k -> ((Type -> Type) -> Type -> Type) -> ((Type -> Type) -> Type -> Type)
data Attenuate orig wrapper :: ((Type -> Type) -> Type -> Type) where
instance Capability orig => Capability (Attenuate orig wrapper) where
  type CapabilityParams (Attenuate orig wrapper) = Core.AttenuateParams (CapabilityParams orig) wrapper

{-
instance Attenuatable '[] where
  attenuate _ _ _ go = go
  unattenuate _ _ _ go = go
instance Attenuatable tl => Attenuatable (MkParam origKey origF : tl) where
  attenuate _ wrapper s go = Core.attenuate origKey origF (origKey, wrapper) (wrapper origF) s (attenuate tl wrapper s go)
  unattenuate _ wrapper s go = Core.unattenuate origKey origF (origKey, wrapper) (wrapper origF) s (unattenuate tl wrapper s go)
-}
{-
type role Drop nominal
type Drop :: forall k. k -> Type
data Drop cap
instance Capability cap => Capability (Drop cap) where
  type CapabilityData (Drop cap) s = Core.Drop (CapabilityData cap s)
  type CapabilityKey (Drop cap) = CapabilityKey cap
type LacksCap cap s = HasCap (Drop cap) s
dropCap :: forall cap -> HasCap cap s => (LacksCap cap s => x) -> x
dropCap cap go = Core.dropCap (CapabilityKey cap) go

type HasCap :: forall k. k -> Type -> Constraint
type HasCap cap s = HasCaps (Sing cap) s


-- Handle
-- IOCap that remembers
-- rename: set key?

type STRefCap :: forall r. Type -> Type -> TYPE r -> TYPE (TupleRep [r, LiftedRep])
newtype STRefCap a s (f :: TYPE r) = MkSTRefCap (# f,  (STRef s a) #)

-}
{-`
module CapIO
  ( CapIO
  , runCapIO
  , IOCap
  , CIO
  , HasIO
  , unliftIO
  , liftIO
  , Capability(..)
  , HasCapability
  , NewSym
  , ValidState
  , AmbientCap
  , HasAmbientCap
  , LacksCapability
  , dropCapability
  , Attenuate
  , attenuate
  , unattenuate
  , PrimCap
  , HasPrim
  , withPrimCap
  , unliftPrim
  , liftPrim
  )
where

-- TODO named caps
import CapIO.Core hiding (unliftPrim, liftPrim, attenuate, unattenuate)
import CapIO.Core qualified as Core
import CapIO.Capabilities


type HasCapability c = IP (NewSym c) c

type HasAmbientCap s = HasCapability (AmbientCap s)

type IOCap = AmbientCap RealWorld
type CIO = CapIO RealWorld
type HasIO = HasCapability IOCap

runCapIO :: (HasCapability IOCap => CIO a) -> IO a
runCapIO = run

unliftIO
  :: HasIO
  => ((forall a. CIO a -> IO a) -> IO b)
  -> CIO b
unliftIO go = Core.unliftPrim (ioToPrim . go . primToIO)

liftIO
  :: HasIO
  => IO a
  -> CIO a
liftIO = Core.liftPrim . ioToPrim

type LacksCapability c = IP (DefaultKey c) (Unbound c)
dropCapability :: forall c -> HasCapability c => (LacksCapability c => x) -> x
dropCapability c = unbind (DefaultKey c)

attenuate
  :: forall c wrapper
  -> ( HasCapability c
     , Coercible c (wrapper c)
     )
  => (HasCapability (Attenuate c wrapper) => x)
  -> x
attenuate c wrapper = Core.attenuate (DefaultKey c) (DefaultKey (Attenuate c wrapper)) wrapper
unattenuate
  :: forall c wrapper
  -> ( HasCapability (Attenuate c wrapper)
     , Coercible (wrapper c) c
     )
  => (HasCapability c => x)
  -> x
unattenuate c wrapper = Core.unattenuate (DefaultKey c) (DefaultKey (Attenuate c wrapper)) wrapper

type role PrimCap nominal nominal
newtype PrimCap (s :: Type) wrapped = MkPrimCap wrapped

instance Capability (PrimCap s wrapped) where
  type WithSt s' (PrimCap s) = PrimCap s'
  type DefaultKey (PrimCap s) = NewSym PrimCap

type HasPrim s = HasCapability (Attenuate (AmbientCap s) (PrimCap s))

withPrimCap :: forall s x. HasAmbientCap s => (HasPrim s => x) -> x
withPrimCap = attenuate (AmbientCap s) (PrimCap s)

unliftPrim :: forall s b. (HasPrim s, ValidState s)
  => (forall m. (PrimBase m, PrimState m ~ s) => (forall a. CapIO s a -> m a) -> m b)
  -> CapIO s b
unliftPrim = unattenuate (AmbientCap s) (PrimCap s) Core.unliftPrim

liftPrim :: forall s a. (HasPrim s, ValidState s)
  => (forall m. (PrimBase m, PrimState m ~ s) => m a)
  -> CapIO s a
liftPrim = unattenuate (AmbientCap s) (PrimCap s) Core.liftPrim

type family HasCapabilities (caps :: [Type]) :: Constraint where
  HasCapabilities [] = ()
  HasCapabilities (hd : tl) = (HasCapability hd, HasCapabilities tl)

type family ChangeSt (s :: Type) (caps :: [Type]) :: [Type] where
  ChangeSt s [] = []
  ChangeSt s (hd : tl) = (WithST s hd) : (ChangeSt s tl)

class Rescope (caps :: [Type]) where
  rescope :: HasCapabilities caps => (forall s. HasCapabilities (ChangeSt s caps) -> x) -> x

instance Rescope [] where
  rescope go = go

-}
