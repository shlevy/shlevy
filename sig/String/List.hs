{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module String.List where

import ASCII.Char qualified as AC
import Char hiding (Sig)
import Char qualified
import Data.Kind
import Data.String (IsString (..))
import String
import "sig" Sig
import Prelude as P hiding (Char)
import Prelude qualified as P

data Impl (g :: Type)

newtype List a = MkList [a]

class ValidChar c where
  fromPrelude :: P.Char -> c
  toPrelude :: c -> P.Char

instance ValidChar AC.Char where
  fromPrelude 'f' = AC.SmallLetterF
  fromPrelude 'o' = AC.SmallLetterO
  fromPrelude _ = error "unimpl"
  toPrelude AC.CapitalLetterF = 'F'
  toPrelude AC.CapitalLetterO = 'O'
  toPrelude _ = error "unimpl"

instance ValidChar c => IsString (List c) where
  fromString = MkList . fmap fromPrelude

deriving via (g :: Type) instance Char.Sig g => Char.Sig (Impl g)

instance (Char.Sig g, ValidChar (Char g)) => Sig (Impl g) where
  type String (Impl g) = List (Char g)
  putStrLn' _ (MkList cs) = withImpl g $ do
    P.putStrLn "list putstrln"
    P.putStrLn (toPrelude <$> cs)
  mapChars' _ f (MkList cs) = MkList (f <$> cs)
