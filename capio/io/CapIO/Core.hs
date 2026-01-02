{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module CapIO.Core where

import CapIO.HasCap
import Data.Coerce
import Control.Monad.Primitive
import Control.Monad.Fix
import Data.Kind

type family CapIO (s :: Type) = (m :: Type -> Type) | m -> s where
  CapIO RealWorld = IO

class (s ~ RealWorld) => ValidState' (s :: Type) where
instance ValidState' RealWorld where
type ValidState (s :: Type) = (ValidState' s, MonadFix (CapIO s))
type HasCap (tag :: k) (s :: Type) = (ValidState s, HasCap' tag s)
type WithCap (tag :: k) s t = (HasCap tag s => t)

data IOCap (s :: Type)

deriving via UncheckedCap IOCap instance Cap IOCap

{-# INLINE runCapIO #-}
runCapIO :: (forall s. WithCap IOCap s (CapIO s a)) -> IO a
runCapIO = \go -> go

{-# INLINE unliftIO #-}
unliftIO :: HasCap IOCap s => ((forall a. CapIO s a -> IO a) -> IO b) -> CapIO s b
unliftIO = \go -> go coerce

{-# INLINE liftIO #-}
liftIO :: HasCap IOCap s => IO a -> CapIO s a
liftIO = coerce

data PrimCap (s :: Type)

deriving via UncheckedCap PrimCap instance Cap PrimCap

{-# INLINE unliftPrim #-}
unliftPrim :: HasCap PrimCap s => (forall m. (PrimBase m, PrimState m ~ s) => (forall a. CapIO s a -> m a) -> m b) -> CapIO s b
unliftPrim = \go -> go coerce

{-# INLINE liftPrim #-}
liftPrim :: HasCap PrimCap s => (forall m. (PrimBase m, PrimState m ~ s) => m a) -> CapIO s a
liftPrim = \go -> go

{-# INLINE primIO #-}
primIO :: HasCap IOCap s => WithCap PrimCap s x -> x
primIO = \go -> go

-- TODO inspection testing
