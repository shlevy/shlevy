{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE QuantifiedConstraints #-}
module CapIO.Core where

import Control.Monad.Fix
import Data.Kind
import Control.Monad.ST
import GHC.Base (IP)
import GHC.Exts
import GHC.IO
import GHC.ST
import Control.Exception.Context

type family AttenuateKey (orig :: Symbol) (desired :: Symbol) :: Symbol where
  AttenuateKey orig _ = orig
type family AttenuateData (orig :: Type) (desired :: Type) :: Type where
  AttenuateData orig _ = orig

attenuate
  :: forall origKey desiredKey desiredTy
  -> (Coercible origTy desiredTy, IP origKey origTy)
  => (IP (AttenuateKey origKey desiredKey) (AttenuateData origTy desiredTy) => x)
  -> x
attenuate _ _ _ = \go -> go

unattenuate
  :: forall origKey desiredKey desiredTy
  -> (Coercible desiredTy origTy, IP (AttenuateKey origKey desiredKey) (AttenuateData origTy desiredTy))
  => (IP origKey origTy => x)
  -> x
unattenuate _ _ _ = \go -> go

type family Drop (t :: Type) = (dt :: Type) | dt -> t where
  Drop t = t

{-# INLINE dropCap #-}
dropCap
  :: forall key
  -> IP key t
  => (IP key (Drop t) => x)
  -> x
dropCap _ = \go -> go

type AmbientCapKey = "exceptionContext"
type AmbientCap = ExceptionContext

withAmbientCap :: (IP AmbientCapKey AmbientCap => x) -> x
withAmbientCap go = go

type family CapIO (s :: Type) = (m :: Type -> Type) | m -> s where
  CapIO RealWorld = IO

class (s ~ RealWorld) => ValidState' (s :: Type)
instance ValidState' RealWorld
class (MonadFix (CapIO s), ValidState' s) => ValidState s
instance ValidState RealWorld

withCoercibleST :: forall s -> (ValidState s, m ~ CapIO s) => ((forall a. Coercible (m a) (ST s a), forall a. (s ~ RealWorld) => Coercible (m a) (IO a)) => x) -> x
withCoercibleST _ = \go -> go
