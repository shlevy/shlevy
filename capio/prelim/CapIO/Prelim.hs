{-# LANGUAGE TypeFamilies #-}

module CapIO.Prelim where

import Data.Kind
import Data.Singletons
import Data.Singletons.Decide
import GHC.TypeLits

data Capability = Root deriving (Eq)

data SCapability :: Capability -> Type where
  SRoot :: SCapability Root

type instance Sing = SCapability

instance SingI Root where
  sing = SRoot

instance SingKind Capability where
  type Demote Capability = Capability
  fromSing SRoot = Root
  toSing Root = SomeSing SRoot

instance SDecide Capability where
  SRoot %~ SRoot = Proved Refl

type NewSym' :: forall k k'. k -> k'
data family NewSym' (t :: k) :: k'

type NewSym :: forall k. k -> Symbol
type NewSym (t :: k) = (NewSym' t :: Symbol)
