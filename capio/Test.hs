{-# LANGUAGE ImplicitParams #-}
module Test where
import Control.Exception.Context
import GHC.Stack
foo :: CallStack
foo = ?bar
