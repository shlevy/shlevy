{-# LANGUAGE UndecidableInstances #-}

module CapIO.Internal where

import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Coerce
import Data.Kind

-- TODO inspect core and inlining

newtype CapST s a = CapST (ST s a) deriving newtype (Functor, Applicative, Monad, MonadFix, Monoid, Semigroup)

type CapIO = CapST RealWorld

type role PureCap nominal

data PureCap (s :: Type) = MkPureCap

runCapST :: (forall s. PureCap s -> CapST s a) -> a
runCapST go = runST (coerce $ go MkPureCap)

data IOCap = MkIOCap

runCapIO :: (IOCap -> CapIO a) -> IO a
runCapIO go = primToIO @(ST RealWorld) (coerce go MkIOCap)

withRunInIO :: IOCap -> ((forall a. CapIO a -> IO a) -> IO b) -> CapIO b
withRunInIO _ go = coerce (ioToPrim @(ST RealWorld) $ (go toIO))
 where
  toIO :: forall a. CapIO a -> IO a
  toIO = coerce (primToIO @(ST RealWorld) @a)

liftIO :: forall a. IOCap -> IO a -> CapIO a
liftIO _ = coerce (ioToPrim @(ST RealWorld) @a)

type role PrimCap nominal

data PrimCap (s :: Type) = MkPrimCap

primIO :: IOCap -> PrimCap RealWorld
primIO _ = MkPrimCap

primPure :: PureCap s -> PrimCap s
primPure _ = MkPrimCap

liftPrim :: forall s a. PrimCap s -> (forall m. (PrimBase m, PrimState m ~ s) => m a) -> CapST s a
liftPrim _ go = coerce (go @(ST s))

withRunInPrim :: forall s b. PrimCap s -> (forall m. (PrimBase m, PrimState m ~ s) => (forall a. CapST s a -> m a) -> m b) -> CapST s b
withRunInPrim _ go = coerce (go toPrim)
 where
  toPrim :: forall a. CapST s a -> ST s a
  toPrim = coerce (stToPrim @(ST s) @a)
