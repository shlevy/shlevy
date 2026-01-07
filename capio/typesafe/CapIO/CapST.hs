{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}

module CapIO.CapST
  ( CapST
  , STCap
  , HasSTCap
  , forgeSTCap
  , sudoST
  , CapIO
  , IOCap
  , HasIOCap
  , sudo
  , forgeIOCap
  , PureCap
  , HasPureCap
  , runCapST
  )
where

import CapIO.Capability
import CapIO.Trustworthy
import Control.Monad.Fix
import Control.Monad.ST
import Data.Coerce
import Data.Kind

newtype CapST s a = MkCapST (ST s a) deriving newtype (Functor, Applicative, Monad, MonadFix)

type role STCap nominal nominal

newtype STCap (s :: Type) a = MkSTCap a

instance Capability (STCap s)

type HasSTCap s = HasCapability (STCap s)

type CapIO = CapST RealWorld

type IOCap = STCap RealWorld

type HasIOCap = HasCapability IOCap

sudo :: (HasIOCap) => ((forall a. Coercible (IO a) (CapIO a)) => x) -> x
sudo @x go = go'
 where
  go' :: (STRealWorld) => x
  go' = go

forgeIOCap :: IO (CapabilityHandle IOCap)
forgeIOCap = pure $ forge (STCap RealWorld)

forgeSTCap :: ST s (CapabilityHandle (STCap s))
forgeSTCap @s = pure $ forge (STCap s)

sudoST :: forall s -> (HasSTCap s) => ((forall a. Coercible (ST s a) (CapST s a)) => x) -> x
sudoST _ go = go

type role PureCap nominal nominal

newtype PureCap (s :: Type) a = MkPureCap a

instance Capability (PureCap s)

type HasPureCap s = HasCapability (PureCap s)

runCapST :: (forall s. (HasCapabilities [PureCap s, STCap s]) => CapST s x) -> x
-- withCapabilities doesn't seem to work with types parameterized by type variables
runCapST go = runST $ \ @s -> withCapabilitiesHandle (forge (PureCap s) :!: forge (STCap s)) (sudoST s (coerce go))
