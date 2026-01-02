{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE QuantifiedConstraints #-}
module CapIO.Core where

import Control.Monad.Fix
import Data.Kind
import Control.Monad.ST
import GHC.Base (IP, ip)
import GHC.Exts
import CapIO.NewSym
import GHC.IO
import GHC.ST

type family AttenuateKey (orig :: Symbol) (desired :: Symbol) :: Symbol where
  AttenuateKey _ desired = desired
type family AttenuateData (orig :: Type) (desired :: Type) :: Type where
  AttenuateData _ desired = desired

attenuate
  :: forall origKey desiredKey desiredTy
  -> (Coercible origTy desiredTy, IP origKey origTy)
  => (IP (AttenuateKey origKey desiredKey) (AttenuateData origTy desiredTy) => x)
  -> x
attenuate origKey desiredKey desiredTy = withDict @(IP desiredKey desiredTy) (coerce (ip @origKey))

unattenuate
  :: forall origKey desiredKey desiredTy
  -> (Coercible desiredTy origTy, IP (AttenuateKey origKey desiredKey) (AttenuateData origTy desiredTy))
  => (IP origKey origTy => x)
  -> x
unattenuate origKey desiredKey _ = withDict @(IP origKey _) (coerce (ip @desiredKey))

type role Drop' nominal
data Drop' (t :: Type) = MkDrop
type family Drop (t :: Type) = (dt :: Type) | dt -> t where
  Drop t = Drop' t

dropCap
  :: forall key
  -> IP key t
  => (IP key (Drop t) => x)
  -> x
dropCap key = withDict @(IP key _) MkDrop

type AmbientCapKey = NewSym AmbientCap
data AmbientCap = MkAmbientCap
withAmbientCap :: (IP AmbientCapKey AmbientCap => x) -> x
withAmbientCap = withDict @(IP AmbientCapKey _) MkAmbientCap

newtype CapIO' s a = CapIO' (ST s a) deriving newtype (Functor, Applicative, Monad, MonadFix)
type family CapIO (s :: Type) = (m :: Type -> Type) | m -> s where
  CapIO s = CapIO' s

class ValidState' (s :: Type)
instance ValidState' s
class (MonadFix (CapIO s), ValidState' s) => ValidState s
instance ValidState s

withCoercibleST :: forall s -> (ValidState s, m ~ CapIO s) => ((forall a. Coercible (m a) (ST s a), forall a. (s ~ RealWorld) => Coercible (m a) (IO a)) => x) -> x
withCoercibleST _ = \go -> go
