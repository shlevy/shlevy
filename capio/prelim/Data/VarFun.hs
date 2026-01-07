{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}

module Data.VarFun
  ( type (~>)
  , VarFun (..)
  , fmapVariable
  , ValidFunArgs
  , FunTy
  , fmapVariable'
  )
where

import Data.Kind
import Data.List (List)

type (~>) t ret = FunTy (FunArgs t) ret

infixr 9 ~>

class (ValidFunArgs (FunArgs t)) => VarFun t where
  type FunArgs t :: List Type
  liftF :: (t -> ret) -> t ~> ret

fmapVariable :: forall t -> (VarFun t) => (x -> y) -> t ~> x -> t ~> y
fmapVariable t = fmapVariable' (FunArgs t)

class ValidFunArgs (args :: List Type) where
  type FunTy args (ret :: Type) :: Type
  fmapVariable' :: forall args' -> (args ~ args') => (x -> y) -> FunTy args x -> FunTy args y

instance ValidFunArgs '[] where
  type FunTy '[] ret = ret
  fmapVariable' _ f = f

instance (ValidFunArgs tl) => ValidFunArgs (hd : tl) where
  type FunTy (hd : tl) ret = hd -> FunTy tl ret
  fmapVariable' _ f toX = fmapVariable' tl f . toX
