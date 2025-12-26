{-# LANGUAGE CPP #-}

module CapIO where

import Control.Exception hiding (bracket)
import Data.Coerce
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

#if !MIN_VERSION_base(4,21,0)
newtype NoBacktrace e = NoBacktrace e deriving (Show)
instance Exception e => Exception (NoBacktrace e) where
  fromException = coerce (fromException @e)
  toException = coerce (toException @e)
  backtraceDesired _ = False
#endif

bracket :: CapIO a -> (a -> Either SomeException b -> CapIO ()) -> (a -> CapIO b) -> CapIO b
bracket acquire release go = mask $ \unmask -> do
  a <- acquire
  b <- catchNoPropagate (unmask $ go a) $ \e@(ExceptionWithContext _ se) -> do
    release a (Left se)
    rethrowIO e
  release a (Right b)
  pure b
 where
#if !MIN_VERSION_base(4,21,0)
    rethrowIO = throwIO . NoBacktrace
    catchNoPropagate = catch
#endif

bracketOnError :: CapIO a -> (a -> SomeException -> CapIO ()) -> (a -> CapIO b) -> CapIO b
bracketOnError acquire release = bracket acquire release'
 where
  release' _ (Right _) = pure ()
  release' a (Left e) = release a e
