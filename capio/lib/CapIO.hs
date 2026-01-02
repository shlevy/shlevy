module CapIO
  ( CapIO
  , runCapIO
  , runCapIOST
  , RootCap
  , forgeRootIO
  , ValidState (..)
  , Capability (..)
  )
where

import CapIO.Core
import CapIO.Prelim
import Control.Monad.ST
import Data.Coerce

runCapIO :: CapIO RealWorld a -> (HasCapability RealWorld Root) => IO a
runCapIO go = sudo RealWorld (coerce go)

runCapIOST :: forall s a. (ValidState s) => CapIO s a -> (HasCapability s Root) => ST s a
runCapIOST go = sudo s (coerce go)
