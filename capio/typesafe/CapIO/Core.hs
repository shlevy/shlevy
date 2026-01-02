{-# LANGUAGE TypeFamilyDependencies #-}

module CapIO.Core
  ( CapIO'
  , CapIO
  , RootCap
  , forgeRootIO
  , ValidState
  , forgeRootST
  )
where

import Control.Monad.Fix
import Control.Monad.ST
import Data.Kind

newtype CapIO' s a = MkCapIO (ST s a) deriving newtype (Functor, Applicative, Monad, MonadFix)

type family CapIO (s :: Type) = (m :: Type -> Type) | m -> s where
  CapIO s = CapIO' s

type role RootCap nominal

data RootCap (s :: Type) = MkRootCap

forgeRootIO :: IO (RootCap RealWorld)
forgeRootIO = pure MkRootCap

class (MonadFix (CapIO s)) => ValidState s where
  forgeRootST :: ST s (RootCap s)

instance ValidState s where
  forgeRootST = pure MkRootCap
