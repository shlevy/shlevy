{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Char.ASCII qualified as CA
import String (putStrLn, toUpperString)
import String qualified
import String.List qualified as SL
import String.Text qualified as ST
import System.Exit
import "sig" Sig
import Prelude hiding (print, putStrLn)

type Si g = (WithImpl g, Sig g)

class (String.Sig (Str1 g), String.Sig (Str2 g)) => Sig g where
  type Str1 g
  type Str2 g

print :: String.Si g => IO ()
print = putStrLn $ toUpperString "foo"

go :: Si g => IO a
go @g = do
  withImpl (Str1 g) print
  withImpl (Str2 g) print
  exitSuccess

main :: IO a
main = withImpl Impl go

data Impl

instance Sig Impl where
  type Str1 Impl = ST.Impl
  type Str2 Impl = SL.Impl CA.Impl
