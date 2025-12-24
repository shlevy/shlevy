module Observe.Event.Core where

import Data.Kind

type EventSelector = Type -> SubEvents -> Type

data SubEvents = NoSubEvents | SubSelector EventSelector

type EventSelectors :: EventSelector -> Type -> Type
data EventSelectors selector field where
  Leaf :: selector field subspec -> EventSelectors selector field
  (:/) :: selector field (SubSelector subsel) -> EventSelectors subsel field' -> EventSelectors selector field'

infixr 5 :/
