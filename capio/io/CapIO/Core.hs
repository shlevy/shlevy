{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module CapIO.Core
  ( CapIO
  , RootCap
  , forgeRootIO
  , HasCapability
  , ValidState (..)
  )
where

import CapIO.Prelim
import Control.Monad.Fix
import Control.Monad.ST
import Data.Coerce
import Data.Kind
import GHC.IO (IO (..))
import GHC.ST (ST (..))

type family CapIO (s :: Type) = (m :: Type -> Type) | m -> s where
  CapIO RealWorld = IO

type role RootCap nominal

data RootCap (s :: Type) = MkRootCap

forgeRootIO :: IO (RootCap RealWorld)
forgeRootIO = pure MkRootCap

type HasCapability :: Type -> Capability -> Constraint
type family HasCapability s cap where
  HasCapability s Root = ()

class (MonadFix (CapIO s)) => ValidState s where
  forgeRootST :: ST s (RootCap s)
  sudo
    :: forall s'
    ->(m ~ CapIO s, s ~ s')
    => ( ( forall a b. (Coercible a b) => Coercible (m a) (ST s b)
         , forall a b. (s ~ RealWorld, Coercible a b) => Coercible (m a) (IO b)
         )
         => x
       )
    -> (HasCapability s Root) => x

instance ValidState RealWorld where
  forgeRootST = pure MkRootCap
  sudo _ go = go
