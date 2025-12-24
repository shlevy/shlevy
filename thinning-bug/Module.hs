module Module where

import Signature qualified as Impl

newtype T = MkT Impl.T
