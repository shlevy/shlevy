{-# LANGUAGE CPP #-}

module CapIO
  ( CapIO
  , IOCap (..)
  , PureCap (..)
  , purity
  , runCapIO
  , runPureIO
  , bracket
  , bracketOnError
  , onException
  , finally
  )
where

import Control.Exception hiding (bracket, bracketOnError, finally, onException)
import Control.Exception qualified as CE
#if !MIN_VERSION_base(4,21,0)
import Data.Coerce
#endif
import System.IO.Unsafe

type CapIO = IO

data IOCap = MkIOCap

data PureCap = MkPureCap

purity :: IOCap -> PureCap
purity _ = MkPureCap

runCapIO :: (IOCap -> CapIO a) -> IO a
runCapIO go = go MkIOCap

runPureIO :: (PureCap -> CapIO a) -> a
runPureIO go = unsafePerformIO (go MkPureCap)

annotateWhileHandling :: SomeException -> IO a -> IO a
#if MIN_VERSION_base(4,21,0)
annotateWhileHandling = annotateIO . WhileHandling
#else
annotateWhileHandling _ x = x

newtype NoBacktrace e = NoBacktrace e deriving (Show)
instance Exception e => Exception (NoBacktrace e) where
  fromException = coerce (fromException @e)
  toException = coerce (toException @e)
  backtraceDesired _ = False
rethrowIO :: (Exception e) => ExceptionWithContext e -> IO a
rethrowIO = throwIO . NoBacktrace
catchNoPropagate :: (Exception e) => IO a -> (e -> IO a) -> IO a
catchNoPropagate = catch
#endif

bracket :: CapIO a -> (a -> Either SomeException b -> CapIO ()) -> (a -> CapIO b) -> CapIO b
bracket acquire release go = mask $ \unmask -> do
  a <- acquire
  b <- catchNoPropagate (unmask $ go a) $ \e@(ExceptionWithContext _ se) -> do
    annotateWhileHandling se $ release a (Left se)
    rethrowIO e
  release a (Right b)
  pure b

bracketOnError :: CapIO a -> (a -> SomeException -> CapIO ()) -> (a -> CapIO b) -> CapIO b
bracketOnError acquire release = bracket acquire release'
 where
  release' _ (Right _) = pure ()
  release' a (Left e) = release a e

onException :: CapIO a -> (SomeException -> CapIO ()) -> CapIO a
onException go cleanup = catchNoPropagate go $ \e@(ExceptionWithContext _ se) -> do
  annotateWhileHandling se $ cleanup se
  rethrowIO e

finally :: CapIO a -> CapIO () -> CapIO a
finally = CE.finally
