{-# LANGUAGE TypeFamilies #-}

module CapIO.Prelim where

import Data.Kind
import GHC.TypeLits

newtype Capability = MkCapability Type

type NewSym' :: forall k k'. k -> k'
data family NewSym' (t :: k) :: k'

type NewSym :: forall k. k -> Symbol
type NewSym (t :: k) = (NewSym' t :: Symbol)
