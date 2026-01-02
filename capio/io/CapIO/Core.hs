{-# LANGUAGE TypeFamilyDependencies #-}

module CapIO.Core
  ( CapIO
  , RootCap
  , forgeRootIO
  , ValidState
  , forgeRootST
  )
where

import Control.Monad.Fix
import Control.Monad.ST
import Data.Kind

type family CapIO (s :: Type) = (m :: Type -> Type) | m -> s where
  CapIO RealWorld = IO

type role RootCap nominal

data RootCap (s :: Type) = MkRootCap

forgeRootIO :: IO (RootCap RealWorld)
forgeRootIO = pure MkRootCap

class (MonadFix (CapIO s)) => ValidState s where
  forgeRootST :: ST s (RootCap s)

instance ValidState RealWorld where
  forgeRootST = pure MkRootCap
