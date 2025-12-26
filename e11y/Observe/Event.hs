{-# LANGUAGE TypeFamilies #-}

module Observe.Event where

import CapIO
import Control.Exception
import Data.Kind

type Spec = Type -> ChildSpec -> Type

data ChildSpec = NoChildren | Children Spec

data ChildBackend :: (Spec -> Type) -> ChildSpec -> Type where
  NoChildrenBackend :: ChildBackend backend NoChildren
  ChildrenBackend :: backend subspec -> ChildBackend backend (Children subspec)

class Backend backend where
  data Event backend :: Type -> Type
  newEvent :: backend selector -> selector field subspec -> CapIO (ChildBackend backend subspec, Event backend field)
  instantEvent :: backend selector -> selector field subspec -> CapIO (ChildBackend backend subspec)
  finalize :: Event backend field -> Maybe SomeException -> CapIO ()
  addField :: Event backend field -> field -> CapIO ()
