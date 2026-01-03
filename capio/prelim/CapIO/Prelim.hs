{-# LANGUAGE TypeFamilies #-}

module CapIO.Prelim where

import GHC.TypeLits

data Capability = Root

type NewSym' :: forall k k'. k -> k'
data family NewSym' (t :: k) :: k'

type NewSym :: forall k. k -> Symbol
type NewSym (t :: k) = (NewSym' t :: Symbol)
