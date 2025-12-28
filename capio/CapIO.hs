{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module CapIO
  ( CapIO
  , IOCap (..)
  , PureCap (..)
  , purity
  , runCapIO
  , runPureIO
  , onException
  , finally
  , with
  , Allocate (..)
  , Restore (..)
  , Allocated (..)
  , ConcurrentAllocate
  , concurrentAllocate
  , runConcurrentAllocate
  , Concurrently (..)
  )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async hiding (Concurrently)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM.TVar
import Control.Exception hiding (bracket, bracketOnError, finally, onException)
import Control.Exception qualified as CE
import Control.Monad
import Control.Monad.STM
import Data.Coerce
import Data.Functor.Compose
import Data.Profunctor
import Data.Profunctor.Cayley
import Data.Void
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

onException :: CapIO a -> (SomeException -> CapIO ()) -> CapIO a
onException go cleanup = catchNoPropagate go $ \e@(ExceptionWithContext _ se) -> do
  annotateWhileHandling se $ cleanup se
  rethrowIO e

finally :: CapIO a -> CapIO () -> CapIO a
finally = CE.finally

with :: Allocate a res -> (res -> CapIO a) -> CapIO a
with alloc body = mask $ \restore' -> do
  let restore = Restore restore'
  MkAllocated{..} <- coerce alloc restore
  x <- restore' (body resource) `onException` (release restore . Left)
  release restore $ Right x
  pure x

newtype Restore = Restore (forall x. CapIO x -> CapIO x)

newtype Allocate b a = MkAllocate (Restore -> CapIO (Allocated b a))
  deriving (Functor) via (Compose ((->) Restore) (Compose CapIO (Allocated b)))
  deriving (Profunctor) via (Cayley (Compose ((->) Restore) CapIO) Allocated)

instance Applicative (Allocate b) where
  pure :: forall a. a -> Allocate b a
  pure = coerce (pure @(Compose ((->) Restore) (Compose CapIO (Allocated b))) @a)
  (<*>) = ap

instance Monad (Allocate b) where
  return = pure
  allocX >>= (f :: a -> Allocate b c) = coerce @(Restore -> CapIO (Allocated b c)) $ \restore -> do
    allocatedX@(MkAllocated (x :: a) releaseX) <- coerce allocX restore
    allocatedY <- coerce f x restore `onException` (releaseX restore . Left)
    pure $ allocatedX *> allocatedY

data Allocated b a = MkAllocated
  { resource :: a
  , release :: Restore -> Either SomeException b -> CapIO ()
  }
  deriving stock (Functor)

instance Profunctor Allocated where
  dimap f g MkAllocated{..} =
    MkAllocated
      { resource = g resource
      , release = \restore -> release restore . fmap f
      }

instance Applicative (Allocated b) where
  pure x = MkAllocated x (\_ _ -> pure ())
  (<*>) = ap

instance Monad (Allocated b) where
  return = pure
  (MkAllocated{..}) >>= f =
    MkAllocated
      { resource = fResource
      , release = \restore b -> fRelease restore b `finally` release restore b
      }
   where
    MkAllocated{resource = fResource, release = fRelease} = f resource

newtype ConcurrentAllocate b a = MkConcurrentAllocate (Allocate Void (STM (Either SomeException (Allocated b a)))) deriving (Functor) via Compose (Allocate Void) (Compose STM (Compose (Either SomeException) (Allocated b)))

concurrentAllocate :: forall b a. Allocate b a -> ConcurrentAllocate b a
concurrentAllocate alloc = coerce @(Restore -> CapIO (Allocated Void (STM (Either SomeException (Allocated b a))))) $ \_ -> do
  needsRelease <- newTVarIO Nothing
  allocationDone <- newTVarIO False
  allocAsync <- asyncWithUnmask $ \restore -> do
    allocated <- coerce alloc (Restore restore)

    nr <- atomically $ do
      writeTVar allocationDone True
      readTVar needsRelease
    case nr of
      Nothing -> pure ()
      Just b -> do
        release allocated (Restore restore) (absurd <$> b)
        allowInterrupt

    pure allocated

  pure $
    MkAllocated
      { resource = waitCatchSTM allocAsync
      , release = \restore b -> do
          ad <- atomically $ do
            writeTVar needsRelease (Just b)
            readTVar allocationDone
          case ad of
            True ->
              uninterruptibleMask_ (waitCatch allocAsync) >>= \case
                Left _ -> pure ()
                Right allocated -> release allocated restore (absurd <$> b)
            False -> void $ forkIO $ cancel allocAsync
      }

instance Applicative (ConcurrentAllocate b) where
  pure :: forall a. a -> ConcurrentAllocate b a
  pure = coerce (pure @(Compose (Allocate Void) (Compose STM (Compose (Either SomeException) (Allocated b)))) @a)
  liftA2 (f :: a -> b' -> c) ca1 ca2 = coerce @(Allocate Void (STM (Either SomeException (Allocated b c)))) $ do
    s1 <- coerce ca1
    s2 <- coerce ca2
    pure $
      (fmap Left <$> s1) <|> (fmap Right <$> s2) >>= \case
        Left e -> pure $ Left e
        Right (Left x) -> fmap (liftA2 f x) <$> s2
        Right (Right y) -> do
          x <- s1
          pure $ liftA2 (liftA2 f) x (pure y)

runConcurrentAllocate :: forall b a. ConcurrentAllocate b a -> Allocate b a
runConcurrentAllocate ca = coerce @(Restore -> CapIO (Allocated b a)) $ \restore -> do
  MkAllocated{..} <- coerce ca restore
  atomically resource `onException` (release restore . Left @_ @Void) >>= \case
    Left e -> do
      annotateWhileHandling e $ release restore (Left e)
      throwIO e
    Right alloced -> pure alloced

newtype Concurrently a = Concurrently {runConcurrently :: CapIO a}
  deriving (Functor) via Async.Concurrently
  deriving (Applicative) via Async.Concurrently
  deriving (Semigroup) via (Async.Concurrently a)
  deriving (Monoid) via (Async.Concurrently a)
