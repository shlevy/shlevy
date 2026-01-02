{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RequiredTypeArguments #-}
module CapIO.Capabilities
  ( Capability(..)
  , HasCapability
  , NewSym
  , bindImplicit
  )
where

import Data.Kind
import GHC.TypeLits
import GHC.Base (IP, withDict)

type DefaultData :: forall k. k -> (Type -> Type)
type family DefaultData c = (f :: Type -> Type) where
  DefaultData c = c
  DefaultData (c :: k) = TypeError (Text "cannot infer default capability data for " :<>: ShowType c :<>: Text "; it must have kind Type -> Type (not " :<>: ShowType k :<>: Text ").")

class Capability (c :: k) where
  type DefaultKey c :: Symbol
  type DefaultKey c = NewSym c
  type Data c (s :: Type) :: Type
  type Data c s = DefaultData c s

type HasCapability c s = IP (DefaultKey c) (Data c s)

bindImplicit :: forall key -> t -> (IP key t => x) -> x
bindImplicit key = withDict @(IP key _)

data family NewSym' (t :: k) :: k'

type NewSym (t :: k) = (NewSym' t :: Symbol)
