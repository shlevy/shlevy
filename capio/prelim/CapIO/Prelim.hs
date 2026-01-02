{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RequiredTypeArguments #-}
module CapIO.Prelim
  ( NewSym
  , Param(..)
  , WithParams
  )
where

import GHC.TypeLits
import Data.Kind
import GHC.Base (IP)
import Data.List (List)

type NewSym' :: forall k k'. k -> k'
data family NewSym' (t :: k) :: k'
type NewSym :: forall k. k -> Symbol
type NewSym (t :: k) = (NewSym' t :: Symbol)

data Param = forall k. MkParam k (Type -> Type)
type family WithParams (params :: List Param) (s :: Type) :: Constraint where
  WithParams '[] _ = ()
  WithParams (MkParam key ty : tl) s = (IP (NewSym key) (ty s), WithParams tl s)
