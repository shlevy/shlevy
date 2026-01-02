{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}
module CapIO.HasCap where

import Data.Kind
import Data.Implicit

-- TODO defaults
class Cap (tag :: k) where
  type HasCap' tag (s :: Type) :: Constraint
  type LacksCap tag (s :: Type) :: Constraint
  attenuate' :: (LacksCap tag s => a) -> a

{-# INLINE attenuate #-}
attenuate :: forall (tag :: k) s -> Cap tag => (LacksCap tag s => a) -> a
attenuate tag s = attenuate' @_ @tag @s

data family UncheckedCap (tag :: k) :: k

instance Cap (UncheckedCap tag) where
  type HasCap' (UncheckedCap tag) s = ()
  type LacksCap (UncheckedCap tag) s = ()
  {-# INLINE attenuate' #-}
  attenuate' = \go -> go

data ImplicitCap (tag :: Type -> Type) :: Type -> Type where

instance Cap (ImplicitCap tag) where
  type HasCap' (ImplicitCap tag) s = ImplicitParameter tag (tag s)
  type LacksCap (ImplicitCap tag) s = ImplicitUnbound tag
  {-# INLINE attenuate' #-}
  attenuate' = unbindImplicit tag
