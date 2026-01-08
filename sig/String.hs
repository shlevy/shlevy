{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}

module String where

import Char (Char, toUpper)
import Char qualified
import Data.Proxy
import Data.String (IsString)
import "sig" Sig
import Prelude hiding (Char, String, putStrLn)

type Si g = (WithImpl g, Sig g)

-- Methods need to take the explicit type argument due to https://gitlab.haskell.org/ghc/ghc/-/issues/26737
class (IsString (String g), Char.Sig g) => Sig g where
  type String g
  putStrLn' :: Proxy g -> String g -> IO ()
  mapChars' :: Proxy g -> (Char g -> Char g) -> (String g -> String g)

putStrLn :: Si g => String g -> IO ()
putStrLn = putStrLn' explImpl

mapChars :: Si g => (Char g -> Char g) -> (String g -> String g)
mapChars = mapChars' explImpl

toUpperString :: Si g => String g -> String g
toUpperString = mapChars toUpper
