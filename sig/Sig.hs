{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}

module Sig where

import Data.Kind
import Data.Proxy
import GHC.Base (IP (..), withDict)

data family Implementation :: k

type WithImpl (g :: Type) = IP Implementation (Proxy g)

withImpl :: forall g -> (WithImpl g => x) -> x
withImpl g = withDict (Proxy @g)

-- Helper to work around https://gitlab.haskell.org/ghc/ghc/-/issues/26737
explImpl :: WithImpl g => Proxy g
explImpl = ip @Implementation
