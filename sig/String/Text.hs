{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module String.Text where

import Char qualified
import Char.UTF qualified as CU
import Data.Text as Text
import Data.Text.IO qualified as TIO
import String
import Prelude qualified as P

data Impl

deriving via CU.Impl instance Char.Sig Impl

instance Sig Impl where
  type String Impl = Text
  putStrLn' _ s = do
    P.putStrLn "text putstrln"
    TIO.putStrLn s
  mapChars' _ = Text.map
