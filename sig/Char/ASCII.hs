{-# LANGUAGE TypeFamilies #-}

module Char.ASCII where

import ASCII.Char qualified as AC
import Char
import Prelude hiding (Char)

data Impl

instance Sig Impl where
  type Char Impl = AC.Char
  toUpper' _ AC.SmallLetterF = AC.CapitalLetterF
  toUpper' _ AC.SmallLetterO = AC.CapitalLetterO
  toUpper' _ _ = error "unimpl"
