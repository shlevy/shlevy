{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Observe.Event where

import CapIO
import Data.Implicit
import Data.Kind
import Data.Profunctor
import Data.Typeable

type Spec = Type -> ChildSpec -> Type

data ChildSpec = NoChildren | Children Spec

class Backend backend where
  data Event backend :: Type -> Type
  allocChildlessEvent :: backend selector -> selector field NoChildren -> Allocate () (Event backend field)
  allocParentEvent :: backend selector -> selector field (Children cs) -> Allocate () (Event backend field, backend cs)
  instantEvent' :: backend selector -> selector field NoChildren -> [field] -> CapIO ()
  addField' :: Event backend field -> field -> CapIO ()

data ComposeBackends spec
  = NoopBackend
  | forall backend. (Backend backend) => (:::) (backend spec) (ComposeBackends spec)

instance Backend ComposeBackends where
  data Event ComposeBackends field
    = NoopEvent
    | forall backend. (Backend backend) => ConsEvent (Event backend field) (Event ComposeBackends field)

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

type HasEvents backend sel = (ImplicitParameter Backend (backend sel), Backend backend)

withBackend :: (Backend backend) => Allocate a (backend sel) -> ((HasEvents backend sel) => CapIO a) -> CapIO a
withBackend alloc go = with alloc (\be -> bindImplicit Backend be go)

instantEvent :: (HasEvents backend sel) => sel field NoChildren -> [field] -> CapIO ()
instantEvent = instantEvent' (implicitParameter Backend)

type family HasChildEvents backend cs where
  HasChildEvents _ NoChildren = ImplicitUnbound Backend
  HasChildEvents backend (Children cs) = ImplicitParameter Backend (backend cs)

type HasEvent backend field = (Backend backend, ImplicitParameter Event (Event backend field))

withEvent :: (HasEvents backend sel, KnownChildSpec cs) => sel field cs -> ((HasEvent backend field, HasChildEvents backend cs) => CapIO a) -> CapIO a
withEvent (sel :: sel field cs) go = case childSpecVal cs of
  NoChildrenVal -> with' (allocChildlessEvent be sel) (\ev -> bindImplicit Event ev (unbindImplicit Backend go))
  ChildrenVal -> with' (allocParentEvent be sel) (\(ev, be') -> bindImplicit Event ev (bindImplicit Backend be' go))
 where
  be = implicitParameter Backend
  with' = with . lmap (const ())

addField :: (HasEvent backend field) => field -> CapIO ()
addField = addField' $ implicitParameter Event

data ChildSpecVal cs where
  NoChildrenVal :: ChildSpecVal NoChildren
  ChildrenVal :: (Typeable s) => ChildSpecVal (Children s)

class KnownChildSpec cs where
  childSpecVal' :: ChildSpecVal cs

instance KnownChildSpec NoChildren where
  childSpecVal' = NoChildrenVal

instance (Typeable s) => KnownChildSpec (Children s) where
  childSpecVal' = ChildrenVal

childSpecVal :: forall cs -> (KnownChildSpec cs) => ChildSpecVal cs
childSpecVal cs = childSpecVal' @cs
