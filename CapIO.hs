{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module CapIO
  ( module CapIO.Core
  , Cleanup (..)
  , onException
  , mask
  , CanCleanup
  , Restore (..)
  , Allocated (..)
  , Allocate (..)
  , with
  , with'
  , CleanupIO (..) -- internals elsewhere
  , canCleanupIO
  , CleanupPure (..) -- internals elsewhere
  , canCleanupPure
  , Clock (..)
  , SystemClock (..) -- internals elsewhere
  , systemClock
  , MonotonicClock (..)
  , monotonicClock
  , MutVarArena (..) -- internals
  , mutVarArena
  , MutVar (..) -- internal
  , newMutVar
  , readMutVar
  , writeMutVar
  , atomicModifyMutVar
  , atomicModifyMutVar'
  , modifyMutVar
  , modifyMutVar'
  )
where

import CapIO.Core (CapIO, ValidState, HasCap, WithCap, IOCap, runCapIO, unliftIO, liftIO, PrimCap, unliftPrim, liftPrim, primIO)
import Control.Exception hiding (mask, onException)
import Control.Exception qualified as CE
import Control.Monad
import Control.Monad.ST
import Data.Coerce
import Data.Functor.Compose
import Data.Implicit
import Data.Primitive.MutVar qualified as Primitive
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock qualified as Time

class Cleanup cleanup where
  type CleanupState cleanup
  onException' :: cleanup -> CapST (CleanupState cleanup) a -> (SomeException -> CapST (CleanupState cleanup) b) -> CapST (CleanupState cleanup) a
  mask' :: cleanup -> ((forall x. CapST (CleanupState cleanup) x -> CapST (CleanupState cleanup) x) -> CapST (CleanupState cleanup) a) -> CapST (CleanupState cleanup) a

newtype CleanupIO = MkCleanupIO IOCap

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

instance Cleanup CleanupIO where
  type CleanupState CleanupIO = RealWorld
  onException' (MkCleanupIO io) body handler = withRunInIO io $ \runCIO ->
    catchNoPropagate (runCIO body) $ \e@(ExceptionWithContext _ se) -> do
      _ <- annotateWhileHandling se . runCIO $ handler se
      -- Maybe: Annotate with the return?
      rethrowIO e
  mask' (MkCleanupIO io) body = withRunInIO io $ \runCIO ->
    CE.mask (\restore -> runCIO (body (\x -> liftIO io (restore (runCIO x)))))

canCleanupIO :: IOCap -> ((CanCleanup CleanupIO) => CapIO a) -> CapIO a
canCleanupIO io go = bindImplicit Cleanup (MkCleanupIO io) go

newtype CleanupPure s = MkCleanupPure (PureCap s)

instance Cleanup (CleanupPure s) where
  type CleanupState (CleanupPure s) = s
  onException' _ body _ = body
  mask' _ body = body id

canCleanupPure :: PureCap s -> ((CanCleanup (CleanupPure s)) => CapST s a) -> CapST s a
canCleanupPure pc go = bindImplicit Cleanup (MkCleanupPure pc) go

type CanCleanup cleanup = (ImplicitParameter Cleanup cleanup, Cleanup cleanup)

onException :: (CanCleanup cleanup) => CapST (CleanupState cleanup) a -> (SomeException -> CapST (CleanupState cleanup) b) -> CapST (CleanupState cleanup) a
onException = onException' (implicitParameter Cleanup)

mask :: (CanCleanup cleanup) => ((forall x. CapST (CleanupState cleanup) x -> CapST (CleanupState cleanup) x) -> CapST (CleanupState cleanup) a) -> CapST (CleanupState cleanup) a
mask = mask' (implicitParameter Cleanup)

data Restore cleanup = (ImplicitParameter Cleanup cleanup) => MkRestore (forall x. CapST (CleanupState cleanup) x -> CapST (CleanupState cleanup) x)

data Allocated cleanup releaseYield releaseArg resource = MkAllocated
  { resource :: resource
  , release :: Restore cleanup -> Either SomeException releaseArg -> CapST (CleanupState cleanup) releaseYield
  }
  deriving stock (Functor)

instance (Monoid releaseYield, Cleanup cleanup) => Applicative (Allocated cleanup releaseYield releaseArg) where
  pure x = MkAllocated x (\_ _ -> pure mempty)
  (<*>) = ap

instance (Monoid releaseYield, Cleanup cleanup) => Monad (Allocated cleanup releaseYield releaseArg) where
  return = pure
  (MkAllocated{..}) >>= f =
    MkAllocated
      { resource = fResource
      , release = \restore@(MkRestore _) b ->
          (<>)
            <$> fRelease restore b `onException` (\_ -> release restore b)
            <*> release restore b
      }
   where
    MkAllocated{resource = fResource, release = fRelease} = f resource

newtype Allocate cleanup releaseYield releaseArg resource = MkAllocate (Restore cleanup -> CapST (CleanupState cleanup) (Allocated cleanup releaseYield releaseArg resource))
  deriving (Functor) via (Compose ((->) (Restore cleanup)) (Compose (CapST (CleanupState cleanup)) (Allocated cleanup releaseYield releaseArg)))

instance (Monoid releaseYield, Cleanup cleanup) => Applicative (Allocate cleanup releaseYield releaseArg) where
  pure :: forall a. a -> Allocate cleanup releaseYield releaseArg a
  pure = coerce (pure @(Compose ((->) (Restore cleanup)) (Compose (CapST (CleanupState cleanup)) (Allocated cleanup releaseYield releaseArg))) @a)
  (<*>) = ap

instance (Monoid releaseYield, Cleanup cleanup) => Monad (Allocate cleanup releaseYield releaseArg) where
  return = pure
  allocX >>= (f :: a -> Allocate cleanup releaseYield releaseArg b) = coerce @(Restore cleanup -> CapST (CleanupState cleanup) (Allocated cleanup releaseYield releaseArg b)) $ \restore@(MkRestore _) -> do
    allocatedX@(MkAllocated (x :: a) releaseX) <- coerce allocX restore
    allocatedY <- coerce f x restore `onException` (releaseX restore . Left)
    pure $ allocatedX *> allocatedY

with' :: (CanCleanup cleanup) => Allocate cleanup b a resource -> (resource -> CapST (CleanupState cleanup) a) -> CapST (CleanupState cleanup) (a, b)
with' alloc body = mask $ \restore' -> do
  let restore = MkRestore restore'
  MkAllocated{..} <- coerce alloc restore
  x <- restore' (body resource) `onException` (release restore . Left)
  ry <- release restore $ Right x
  pure (x, ry)

with :: (CanCleanup cleanup) => Allocate cleanup b a resource -> (resource -> CapST (CleanupState cleanup) a) -> CapST (CleanupState cleanup) a
with alloc = fmap fst . with' alloc

class Clock clock where
  type ClockState clock
  getCurrentTime :: clock -> CapST (ClockState clock) UTCTime

newtype SystemClock = MkSystemClock IOCap

instance Clock SystemClock where
  type ClockState SystemClock = RealWorld
  getCurrentTime (MkSystemClock io) = liftIO io Time.getCurrentTime

systemClock :: IOCap -> SystemClock
systemClock = MkSystemClock

newtype MonotonicClock s = MkMonotonicClock (MutVar s UTCTime)

instance Clock (MonotonicClock s) where
  type ClockState (MonotonicClock s) = s
  getCurrentTime (MkMonotonicClock clk) = atomicModifyMutVar clk (\now -> (Time.addUTCTime 1 now, now))

monotonicClock :: MutVarArena s -> UTCTime -> CapST s (MonotonicClock s)
monotonicClock arena = coerce (newMutVar @_ @UTCTime arena)

newtype MutVarArena s = MkMutVarArena (PrimCap s)

mutVarArena :: PrimCap s -> MutVarArena s
mutVarArena = MkMutVarArena

data MutVar s a = MkMutVar (PrimCap s) (Primitive.MutVar s a)

newMutVar :: MutVarArena s -> a -> CapST s (MutVar s a)
newMutVar (MkMutVarArena pc) a = MkMutVar pc <$> liftPrim pc (Primitive.newMutVar a)

readMutVar :: MutVar s a -> CapST s a
readMutVar (MkMutVar pc mv) = liftPrim pc (Primitive.readMutVar mv)

writeMutVar :: MutVar s a -> a -> CapST s ()
writeMutVar (MkMutVar pc mv) a = liftPrim pc (Primitive.writeMutVar mv a)

atomicModifyMutVar :: MutVar s a -> (a -> (a, b)) -> CapST s b
atomicModifyMutVar (MkMutVar pc mv) f = liftPrim pc (Primitive.atomicModifyMutVar mv f)

atomicModifyMutVar' :: MutVar s a -> (a -> (a, b)) -> CapST s b
atomicModifyMutVar' (MkMutVar pc mv) f = liftPrim pc (Primitive.atomicModifyMutVar' mv f)

modifyMutVar :: MutVar s a -> (a -> a) -> CapST s ()
modifyMutVar (MkMutVar pc mv) f = liftPrim pc (Primitive.modifyMutVar mv f)

modifyMutVar' :: MutVar s a -> (a -> a) -> CapST s ()
modifyMutVar' (MkMutVar pc mv) f = liftPrim pc (Primitive.modifyMutVar' mv f)
