{-# LANGUAGE TypeFamilies #-}

module Observe.Event where

import CapIO
import Control.Exception
import Data.Kind

type EventSelector = Type -> SubEvents -> Type

data SubEvents = NoSubEvents | SubSelector EventSelector

type EventSelectors :: EventSelector -> Type -> Type
data EventSelectors selector field where
  Leaf :: selector field subspec -> EventSelectors selector field
  (:/) :: selector field (SubSelector subsel) -> EventSelectors subsel field' -> EventSelectors selector field'

infixr 5 :/

class Event ev where
  type EventReference ev
  reference :: ev field -> EventReference ev
  finalize :: ev field -> Maybe SomeException -> CapIO ()
  addField :: ev field -> field -> CapIO ()

data EventParams backend selector field = EventParams
  { selectors :: !(EventSelectors selector field)
  , parent :: !(Maybe (EventReference (BackendEvent backend)))
  , causes :: ![EventReference (BackendEvent backend)]
  , initialFields :: ![field]
  }

class (Event (BackendEvent backend)) => EventBackend backend where
  type BackendEvent backend :: Type -> Type

  newEvent :: backend selector -> EventParams backend selector field -> CapIO (BackendEvent backend field)
  newInstantEvent :: backend selector -> EventParams backend selector field -> CapIO (EventReference (BackendEvent backend))
