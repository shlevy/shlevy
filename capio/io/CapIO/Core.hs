{-# LANGUAGE QuantifiedConstraints #-}

module CapIO.Core
  ( CapIO
  , IOCap
  , HasIOCap
  , sudo
  , forgeIOCap
  )
where

import CapIO.Capability
import Data.Coerce

type CapIO = IO

type role IOCap nominal

newtype IOCap a = MkIOCap a

instance Capability IOCap

type HasIOCap = HasCapability IOCap

sudo :: (HasIOCap) => ((forall a. Coercible (IO a) (CapIO a)) => x) -> x
sudo go = go

forgeIOCap :: IO (CapabilityHandle IOCap)
forgeIOCap = pure $ forge IOCap
