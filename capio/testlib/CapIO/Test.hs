{-# LANGUAGE ExplicitNamespaces #-}
module CapIO.Test where

import CapIO.Core
import System.Exit
import Data.Functor.Identity
import Data.Coerce
import GHC.Base (IP)
import Data.STRef
import Control.Monad.ST

securedSecuredExitCode :: IP AmbientCapKey AmbientCap => ExitCode
securedSecuredExitCode = ExitSuccess

securedExitCode :: IP (AttenuateKey AmbientCapKey "foo") (AttenuateData AmbientCap (Identity AmbientCap)) => ExitCode
securedExitCode = unattenuate AmbientCapKey "foo" (type (Identity AmbientCap)) securedSecuredExitCode

permissionlessExitCode :: IP AmbientCapKey (Drop AmbientCap) => ExitCode
permissionlessExitCode = withAmbientCap (attenuate AmbientCapKey "foo" (type (Identity AmbientCap)) securedExitCode)

exitCode :: ExitCode
exitCode = withAmbientCap (dropCap AmbientCapKey permissionlessExitCode)

run :: forall s. ValidState s => CapIO s ExitCode
run = withCoercibleST s $ do
  var <- coerce @(ExitCode -> ST s (STRef s ExitCode)) @(ExitCode -> CapIO s (STRef s ExitCode)) newSTRef exitCode
  coerce @(STRef s ExitCode -> ST s ExitCode) @(STRef s ExitCode -> CapIO s ExitCode) readSTRef var

testMain :: IO ()
testMain = withCoercibleST RealWorld $ do
  code <- stToIO (coerce @(CapIO RealWorld ExitCode) run)
  exitWith code
