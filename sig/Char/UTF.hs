{-# LANGUAGE TypeFamilies #-}

module Char.UTF where

import Char
import Data.Char qualified as C
import Prelude hiding (Char)
import Prelude qualified as P

data Impl

instance Sig Impl where
  type Char Impl = P.Char
  toUpper' _ = C.toUpper
