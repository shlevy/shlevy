{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module CapIO.Core
  ( CapIO
  , ValidState'
  , ValidState
  , HasCap
  , WithCap
  , IOCap
  , runCapIO
  , unliftIO
  , liftIO
  , PrimCap
  , unliftPrim
  , liftPrim
  , primIO
  , PureCap
  , primPure
  , runPure
  )
where

import Control.Monad.Primitive hiding (liftPrim)
import Control.Monad.Fix
import Data.Kind
import CapIO.HasCap
import Control.Monad.ST
import Data.Implicit
import Data.Coerce

newtype CapIO' s a = CapIO' (ST s a) deriving newtype (Functor, Applicative, Monad, MonadFix)

type family CapIO (s :: Type) = (m :: Type -> Type) | m -> s where
  CapIO s = CapIO' s

class ValidState' (s :: Type) where
instance ValidState' (s :: Type) where
type ValidState (s :: Type) = (ValidState' s, MonadFix (CapIO s))
type HasCap (tag :: k) (s :: Type) = (ValidState s, HasCap' tag s)
type WithCap (tag :: k) s t = (HasCap tag s => t)

type role IOCap nominal
data IOCap (s :: Type) where
  MkIOCap :: IOCap RealWorld

deriving via ImplicitCap IOCap instance Cap IOCap

runCapIO :: forall a. (forall s. WithCap IOCap s (CapIO s a)) -> IO a
runCapIO go = coerce @(ST RealWorld a -> IO a) primToIO (bindImplicit IOCap MkIOCap go)

unliftIO :: forall s b. HasCap IOCap s => ((forall a. CapIO s a -> IO a) -> IO b) -> CapIO s b
unliftIO go = case implicitParameter IOCap of
    MkIOCap -> coerce @(IO b -> ST RealWorld b) ioToPrim (go primToIO')
  where
    primToIO' :: forall a. CapIO RealWorld a -> IO a
    primToIO' = coerce @(ST RealWorld a -> IO a) primToIO

liftIO :: HasCap IOCap s => IO a -> CapIO s a
liftIO (io :: IO a) = case implicitParameter IOCap of
  MkIOCap -> coerce @(IO a -> ST RealWorld a) ioToPrim io

type role PrimCap nominal
data PrimCap (s :: Type) = MkPrimCap

deriving via ImplicitCap PrimCap instance Cap PrimCap

unliftPrim :: forall s b. HasCap PrimCap s => (forall m. (PrimBase m, PrimState m ~ s) => (forall a. CapIO s a -> m a) -> m b) -> CapIO s b
unliftPrim go = coerce (go @(ST s) coerce)

liftPrim :: HasCap PrimCap s => (forall m. (PrimBase m, PrimState m ~ s) => m a) -> CapIO s a
liftPrim go = coerce @(ST _ _) go

primIO :: HasCap IOCap s => WithCap PrimCap s x -> x
primIO go = bindImplicit PrimCap MkPrimCap go

type role PureCap nominal
data PureCap (s :: Type) = MkPureCap

deriving via ImplicitCap PureCap instance Cap PureCap

primPure :: HasCap PureCap s => WithCap PrimCap s x -> x
primPure go = bindImplicit PrimCap MkPrimCap go

runPure :: forall a. (forall s. HasCap PureCap s => CapIO s a) -> a
runPure go = runST (coerce go')
  where
    go' :: forall s. CapIO s a
    go' = bindImplicit PureCap (MkPureCap :: PureCap s) go
