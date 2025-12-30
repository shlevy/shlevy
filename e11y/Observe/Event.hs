{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Observe.Event where

import CapIO
import Control.Exception (displayException)
import Data.Coerce
import Data.Constraint
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Product
import Data.Implicit
import Data.Kind
import Data.Time.Clock (UTCTime)
import Data.Type.Equality
import Data.Typeable

type Spec = Type -> ChildSpec -> Type

data ChildSpec = NoChildren | Children Spec

data IfChildren cs thn els where
  ThenChildren :: thn s -> IfChildren (Children s) thn els
  ElseChildren :: els -> IfChildren NoChildren thn els

deriving stock instance (Show els) => Show (IfChildren NoChildren thn els)

deriving stock instance (Show (thn s)) => Show (IfChildren (Children s) thn els)

deriving stock instance (Eq els) => Eq (IfChildren NoChildren thn els)

deriving stock instance (Eq (thn s)) => Eq (IfChildren (Children s) thn els)

class (Cleanup (BackendCleanup backend)) => Backend backend where
  type BackendCleanup backend
  data Event backend :: Type -> Type
  allocChildlessEvent :: backend selector -> selector field NoChildren -> Allocate (BackendCleanup backend) () a (Event backend field)
  allocParentEvent :: backend selector -> selector field (Children cs) -> Allocate (BackendCleanup backend) () a (Event backend field, backend cs)
  instantEvent' :: backend selector -> selector field NoChildren -> [field] -> CapST (BackendState backend) ()
  addField' :: Event backend field -> field -> CapST (BackendState backend) ()

type BackendState backend = CleanupState (BackendCleanup backend)

data ComposeBackends cleanup spec where
  NoopBackend :: ComposeBackends cleanup spec
  (:::) :: (Backend backend) => backend spec -> ComposeBackends (BackendCleanup backend) spec -> ComposeBackends (BackendCleanup backend) spec

instance (Cleanup cleanup) => Backend (ComposeBackends cleanup) where
  type BackendCleanup (ComposeBackends cleanup) = cleanup
  data Event (ComposeBackends cleanup) field where
    NoopEvent :: Event (ComposeBackends cleanup) field
    ConsEvent :: (Backend backend) => Event backend field -> Event (ComposeBackends (BackendCleanup backend)) field -> Event (ComposeBackends (BackendCleanup backend)) field

  allocChildlessEvent NoopBackend _ = pure NoopEvent
  allocChildlessEvent (hd ::: tl) sel = ConsEvent <$> allocChildlessEvent hd sel <*> allocChildlessEvent tl sel

  allocParentEvent NoopBackend _ = pure (NoopEvent, NoopBackend)
  allocParentEvent (hd ::: tl) sel = do
    (hdEv, hdCb) <- allocParentEvent hd sel
    (tlEv, tlCb) <- allocParentEvent tl sel
    pure (ConsEvent hdEv tlEv, hdCb ::: tlCb)

  instantEvent' NoopBackend _ _ = pure ()
  instantEvent' (hd ::: tl) sel fields = instantEvent' hd sel fields *> instantEvent' tl sel fields

  addField' NoopEvent _ = pure ()
  addField' (ConsEvent hd tl) field = addField' hd field *> addField' tl field

type HasEvents backend sel = (ImplicitParameter Backend (backend sel), Backend backend, ImplicitParameter Cleanup (BackendCleanup backend))

withBackend :: (Backend backend, ImplicitParameter Cleanup (BackendCleanup backend)) => Allocate (BackendCleanup backend) b a (backend sel) -> ((HasEvents backend sel) => CapST (BackendState backend) a) -> CapST (BackendState backend) a
withBackend alloc go = fst <$> withBackend' alloc go

withBackend' :: (Backend backend, ImplicitParameter Cleanup (BackendCleanup backend)) => Allocate (BackendCleanup backend) b a (backend sel) -> ((HasEvents backend sel) => CapST (BackendState backend) a) -> CapST (BackendState backend) (a, b)
withBackend' alloc go = with' alloc (\be -> bindImplicit Backend be go)

instantEvent :: (HasEvents backend sel) => sel field NoChildren -> [field] -> CapST (BackendState backend) ()
instantEvent = instantEvent' (implicitParameter Backend)

type family HasChildEvents backend cs where
  HasChildEvents _ NoChildren = ImplicitUnbound Backend
  HasChildEvents backend (Children cs) = (ImplicitParameter Backend (backend cs), ImplicitParameter Cleanup (BackendCleanup backend))

type HasEvent backend field = (Backend backend, ImplicitParameter Event (Event backend field))

withEvent :: (HasEvents backend sel, KnownChildSpec cs) => sel field cs -> ((HasEvent backend field, HasChildEvents backend cs) => CapST (BackendState backend) a) -> CapST (BackendState backend) a
withEvent (sel :: sel field cs) go = case childSpecVal cs of
  NoChildrenVal -> with (allocChildlessEvent be sel) (\ev -> bindImplicit Event ev (unbindImplicit Backend go))
  ChildrenVal -> with (allocParentEvent be sel) (\(ev, be') -> bindImplicit Event ev (bindImplicit Backend be' go))
 where
  be = implicitParameter Backend

addField :: (HasEvent backend field) => field -> CapST (BackendState backend) ()
addField = addField' $ implicitParameter Event

data ChildSpecVal cs where
  NoChildrenVal :: ChildSpecVal NoChildren
  ChildrenVal :: ChildSpecVal (Children s)

class KnownChildSpec cs where
  childSpecVal' :: ChildSpecVal cs

instance KnownChildSpec NoChildren where
  childSpecVal' = NoChildrenVal

instance KnownChildSpec (Children s) where
  childSpecVal' = ChildrenVal

childSpecVal :: forall cs -> (KnownChildSpec cs) => ChildSpecVal cs
childSpecVal cs = childSpecVal' @cs

data EventTimestamp = EventStart UTCTime | EventEnd UTCTime deriving stock (Eq, Show)

data TimestampEvents selector :: Spec where
  TimestampChildless :: selector field NoChildren -> TimestampEvents selector (Either EventTimestamp field) NoChildren
  TimestampParent :: selector field (Children cs) -> TimestampEvents selector (Either EventTimestamp field) (Children (TimestampEvents cs))

deriving instance (forall f' cs'. Show (selector f' cs')) => Show (TimestampEvents selector f cs)

deriving instance (forall f' cs'. Eq (selector f' cs')) => Eq (TimestampEvents selector f cs)

data TimestampEventsBackend clock backend selector = MkTimestampEventsBackend
  { underlying :: backend (TimestampEvents selector)
  , clk :: clock
  }

allocTimestampEventsBackend :: (HasEvents backend (TimestampEvents selector)) => clock -> Allocate (BackendCleanup backend) () a (TimestampEventsBackend clock backend selector)
allocTimestampEventsBackend = pure . MkTimestampEventsBackend (implicitParameter Backend)

allocTimestampEvent :: (Clock clock, Backend backend, BackendState backend ~ ClockState clock) => clock -> Event backend (Either EventTimestamp field) -> Allocate (BackendCleanup backend) () a (Event (TimestampEventsBackend clock backend) field)
allocTimestampEvent clk ev = MkAllocate $ \(MkRestore restore) -> restore $ do
  getCurrentTime clk >>= addField' ev . Left . EventStart
  let
    resource = TimestampEventsEvent ev
    release (MkRestore restore') _ =
      restore' $
        getCurrentTime clk >>= addField' ev . Left . EventEnd
  pure $ MkAllocated{..}

instance (Backend backend, Clock clock, BackendState backend ~ ClockState clock) => Backend (TimestampEventsBackend clock backend) where
  type BackendCleanup (TimestampEventsBackend clock backend) = BackendCleanup backend
  newtype Event (TimestampEventsBackend clock backend) field = TimestampEventsEvent (Event backend (Either EventTimestamp field))
  addField' (TimestampEventsEvent ev) = addField' ev . Right
  allocChildlessEvent be sel = do
    ev <- allocChildlessEvent be.underlying (TimestampChildless sel)
    allocTimestampEvent be.clk ev
  allocParentEvent be sel = do
    (ev, underlying) <- allocParentEvent be.underlying (TimestampParent sel)
    ev' <- allocTimestampEvent be.clk ev
    pure (ev', MkTimestampEventsBackend{underlying, clk = be.clk})
  instantEvent' be sel fields = do
    now <- getCurrentTime be.clk
    instantEvent' be.underlying (TimestampChildless sel) $ (Left $ EventStart now) : (Right <$> fields)

data TimestampFields selector :: Spec where
  TimestampChildlessFields :: selector field NoChildren -> TimestampFields selector (UTCTime, field) NoChildren
  TimestampParentFields :: selector field (Children cs) -> TimestampFields selector (UTCTime, field) (Children (TimestampFields cs))

deriving instance (forall f' cs'. Show (selector f' cs')) => Show (TimestampFields selector f cs)

deriving instance (forall f' cs'. Eq (selector f' cs')) => Eq (TimestampFields selector f cs)

data TimestampFieldsBackend clock backend selector = MkTimestampFieldsBackend
  { underlying :: backend (TimestampFields selector)
  , clk :: clock
  }

instance (Backend backend, Clock clock, BackendState backend ~ ClockState clock) => Backend (TimestampFieldsBackend clock backend) where
  type BackendCleanup (TimestampFieldsBackend clock backend) = BackendCleanup backend
  data Event (TimestampFieldsBackend clock backend) field = TimestampFieldsEvent clock (Event backend (UTCTime, field))
  addField' (TimestampFieldsEvent clk ev) field = do
    now <- getCurrentTime clk
    addField' ev (now, field)
  allocChildlessEvent be sel =
    TimestampFieldsEvent be.clk
      <$> allocChildlessEvent be.underlying (TimestampChildlessFields sel)
  allocParentEvent be sel = do
    (ev, underlying) <- allocParentEvent be.underlying (TimestampParentFields sel)
    pure (TimestampFieldsEvent be.clk ev, MkTimestampFieldsBackend{underlying, clk = be.clk})
  instantEvent' be sel fields = do
    now <- getCurrentTime be.clk
    instantEvent' be.underlying (TimestampChildlessFields sel) $ ((now,) <$> fields)

data ReplayBackend cleanup sel = MkReplayBackend
  { arena :: MutVarArena (CleanupState cleanup)
  , eventsQ :: MutVar (CleanupState cleanup) [ReplayEvent sel]
  }

allocReplayBackend :: MutVarArena (CleanupState cleanup) -> Allocate cleanup [ReplayEvent sel] a (ReplayBackend cleanup sel)
allocReplayBackend arena = MkAllocate $ \(MkRestore restore) -> restore $ do
  eventsQ <- newMutVar arena []
  pure $
    MkAllocated
      { release = \_ _ -> reverse <$> (readMutVar eventsQ)
      , resource = MkReplayBackend{..}
      }

data DurationAndException = Instant | Extended (Maybe SomeException) deriving stock (Show)

eqishExceptions :: Maybe SomeException -> Maybe SomeException -> Bool
eqishExceptions Nothing Nothing = True
eqishExceptions (Just (SomeException (e1 :: et1))) (Just (SomeException (e2 :: et2))) = case eqT @et1 @et2 of
  Nothing -> False
  Just Refl -> displayException e1 == displayException e2
eqishExceptions _ _ = False

eqishDurationAndException :: DurationAndException -> DurationAndException -> Bool
eqishDurationAndException Instant Instant = True
eqishDurationAndException (Extended me1) (Extended me2) = eqishExceptions me1 me2
eqishDurationAndException _ _ = False

data ReplayEvent sel = forall field cs. MkReplayEvent
  { eventSelector :: sel field cs
  , fields :: [field]
  , childrenAndException :: IfChildren cs (Product (Const (Maybe SomeException)) (Compose [] ReplayEvent)) DurationAndException
  }

class (forall field cs. c (spec field cs)) => FieldsAndSelectorsMeet c spec where
  cSpec :: spec field cs -> (Dict (c field), IfChildren cs (Compose Dict (FieldsAndSelectorsMeet c)) ())

instance (FieldsAndSelectorsMeet Show spec) => Show (ReplayEvent spec) where
  showsPrec _ (MkReplayEvent{..}) = case cSpec @Show eventSelector of
    (Dict, ic) ->
      showString "MkReplayEvent "
        . showChar '{'
        . showString "eventSelector = "
        . shows eventSelector
        . showString ", fields = "
        . shows fields
        . showString ", childrenAndException = "
        . ( case ic of
              ThenChildren (Compose Dict) -> shows childrenAndException
              ElseChildren _ -> shows childrenAndException
          )
        . showChar '}'

instance (FieldsAndSelectorsMeet Show spec) => FieldsAndSelectorsMeet Show (TimestampEvents spec) where
  cSpec (TimestampChildless sel) = case cSpec @Show sel of
    (Dict, ElseChildren ()) -> (Dict, ElseChildren ())
  cSpec (TimestampParent sel) = case cSpec @Show sel of
    (Dict, ThenChildren (Compose Dict)) -> (Dict, ThenChildren (Compose Dict))

instance (FieldsAndSelectorsMeet Eq spec) => FieldsAndSelectorsMeet Eq (TimestampEvents spec) where
  cSpec (TimestampChildless sel) = case cSpec @Eq sel of
    (Dict, ElseChildren ()) -> (Dict, ElseChildren ())
  cSpec (TimestampParent sel) = case cSpec @Eq sel of
    (Dict, ThenChildren (Compose Dict)) -> (Dict, ThenChildren (Compose Dict))

instance (FieldsAndSelectorsMeet Show spec) => FieldsAndSelectorsMeet Show (TimestampFields spec) where
  cSpec (TimestampChildlessFields sel) = case cSpec @Show sel of
    (Dict, ElseChildren ()) -> (Dict, ElseChildren ())
  cSpec (TimestampParentFields sel) = case cSpec @Show sel of
    (Dict, ThenChildren (Compose Dict)) -> (Dict, ThenChildren (Compose Dict))

instance (FieldsAndSelectorsMeet Eq spec) => FieldsAndSelectorsMeet Eq (TimestampFields spec) where
  cSpec (TimestampChildlessFields sel) = case cSpec @Eq sel of
    (Dict, ElseChildren ()) -> (Dict, ElseChildren ())
  cSpec (TimestampParentFields sel) = case cSpec @Eq sel of
    (Dict, ThenChildren (Compose Dict)) -> (Dict, ThenChildren (Compose Dict))

class TestSpecEquality spec where
  testSpecEquality :: spec f1 cs1 -> spec f2 cs2 -> Maybe (f1 :~: f2, cs1 :~: cs2, IfChildren cs1 (Compose Dict TestSpecEquality) ())

instance (TestSpecEquality spec) => TestSpecEquality (TimestampEvents spec) where
  testSpecEquality (TimestampChildless sel1) (TimestampChildless sel2) = case testSpecEquality sel1 sel2 of
    Nothing -> Nothing
    Just (Refl, Refl, _) -> Just (Refl, Refl, ElseChildren ())
  testSpecEquality (TimestampParent sel1) (TimestampParent sel2) = case testSpecEquality sel1 sel2 of
    Nothing -> Nothing
    Just (Refl, Refl, ThenChildren (Compose Dict)) -> Just (Refl, Refl, ThenChildren (Compose Dict))
  testSpecEquality _ _ = Nothing

instance (TestSpecEquality spec) => TestSpecEquality (TimestampFields spec) where
  testSpecEquality (TimestampChildlessFields sel1) (TimestampChildlessFields sel2) = case testSpecEquality sel1 sel2 of
    Nothing -> Nothing
    Just (Refl, Refl, _) -> Just (Refl, Refl, ElseChildren ())
  testSpecEquality (TimestampParentFields sel1) (TimestampParentFields sel2) = case testSpecEquality sel1 sel2 of
    Nothing -> Nothing
    Just (Refl, Refl, ThenChildren (Compose Dict)) -> Just (Refl, Refl, ThenChildren (Compose Dict))
  testSpecEquality _ _ = Nothing

instance (FieldsAndSelectorsMeet Eq spec, TestSpecEquality spec) => Eq (ReplayEvent spec) where
  MkReplayEvent{eventSelector = es1, fields = fs1, childrenAndException = cs1} == MkReplayEvent{eventSelector = es2, fields = fs2, childrenAndException = cs2} = case testSpecEquality es1 es2 of
    Nothing -> False
    Just (Refl, Refl, testKids) -> case cSpec @Eq es1 of
      (Dict, ic) ->
        (es1 == es2)
          && (fs1 == fs2)
          && ( case (ic, testKids) of
                 (ThenChildren (Compose Dict), ThenChildren (Compose Dict)) ->
                   let
                     ThenChildren (Pair (Const me1) (Compose events1)) = cs1
                     ThenChildren (Pair (Const me2) (Compose events2)) = cs2
                    in
                     events1 == events2 && eqishExceptions me1 me2
                 (ElseChildren _, ElseChildren _) ->
                   let
                     ElseChildren de1 = cs1
                     ElseChildren de2 = cs2
                    in
                     eqishDurationAndException de1 de2
             )

replayEvents :: (HasEvents backend sel) => [ReplayEvent sel] -> CapST (BackendState backend) ()
replayEvents [] = pure ()
replayEvents (MkReplayEvent{..} : tl) = do
  _ :: () <- case childrenAndException of
    ThenChildren (Pair (Const e) (Compose children)) -> mask $ \restore -> do
      let
        MkAllocate alloc = allocParentEvent (implicitParameter Backend) eventSelector
        restore' = MkRestore restore
      MkAllocated{..} <- alloc restore'
      let
        (ev, cbe) = resource
        populateEvent = do
          restore $ bindImplicit Backend cbe (replayEvents children)
          restore $ traverse_ (addField' ev) fields
          release restore' $ maybe (Right ()) Left e
      populateEvent `onException` (release restore' . Left)
    ElseChildren Instant -> instantEvent eventSelector fields
    ElseChildren (Extended e) -> mask $ \restore -> do
      let
        MkAllocate alloc = allocChildlessEvent (implicitParameter Backend) eventSelector
        restore' = MkRestore restore
      MkAllocated{..} <- alloc restore'
      let
        populateEvent = do
          restore $ traverse_ (addField' resource) fields
          release restore' $ maybe (Right ()) Left e
      populateEvent `onException` (release restore' . Left)
  replayEvents tl

instance (Cleanup cleanup) => Backend (ReplayBackend cleanup) where
  type BackendCleanup (ReplayBackend cleanup) = cleanup
  newtype Event (ReplayBackend cleanup) field = InflightReplayEvent
    {fieldsQ :: MutVar (CleanupState cleanup) [field]}
  addField' ire field = atomicModifyMutVar ire.fieldsQ (\tl -> (field : tl, ()))

  instantEvent' be eventSelector fields = atomicModifyMutVar be.eventsQ (\tl -> (hd : tl, ()))
   where
    hd =
      MkReplayEvent
        { childrenAndException = ElseChildren Instant
        , ..
        }

  allocChildlessEvent be eventSelector = MkAllocate $ \(MkRestore restore) -> restore $ do
    fieldsQ <- newMutVar be.arena []
    pure $
      MkAllocated
        { release = \_ b -> do
            fields <- reverse <$> (readMutVar fieldsQ)
            let
              hd =
                MkReplayEvent
                  { childrenAndException = ElseChildren (Extended $ either Just (const Nothing) b)
                  , ..
                  }
            atomicModifyMutVar be.eventsQ (\tl -> (hd : tl, ()))
        , resource = InflightReplayEvent{..}
        }

  allocParentEvent be eventSelector = MkAllocate $ \(MkRestore restore) -> restore $ do
    fieldsQ <- newMutVar be.arena []
    eventsQ <- newMutVar be.arena []
    pure $
      MkAllocated
        { release = \_ b -> do
            fields <- reverse <$> (readMutVar fieldsQ)
            children <- reverse <$> (readMutVar eventsQ)
            let
              hd =
                MkReplayEvent
                  { childrenAndException = ThenChildren (Pair (coerce $ either Just (const Nothing) b) (coerce children))
                  , ..
                  }
            atomicModifyMutVar be.eventsQ (\tl -> (hd : tl, ()))
        , resource = (InflightReplayEvent{..}, be{eventsQ})
        }
