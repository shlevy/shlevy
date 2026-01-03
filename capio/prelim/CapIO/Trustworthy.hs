{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Trustworthy #-}

module CapIO.Trustworthy
  ( STRealWorld
  , IP
  , ip
  , bindImplicit
  )
where

import Control.Monad.ST (RealWorld)
import Data.Coerce
import GHC.Base (IP, ip, withDict)
import GHC.IO (IO (..))
import GHC.ST (ST (..))

class (forall a b. (Coercible a b) => Coercible (IO a) (ST RealWorld b)) => STRealWorld

instance STRealWorld

bindImplicit :: t -> ((IP k t) => x) -> x
bindImplicit = withDict
