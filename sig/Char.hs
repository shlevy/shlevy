{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}

module Char where

import Data.Proxy
import "sig" Sig
import Prelude hiding (Char)

type Si g = (WithImpl g, Sig g)

-- Methods need to take the explicit type argument due to https://gitlab.haskell.org/ghc/ghc/-/issues/26737
class Sig g where
  type Char g
  toUpper' :: Proxy g -> Char g -> Char g

toUpper :: Si g => Char g -> Char g
toUpper = toUpper' explImpl
