{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Manage the implicit parameter namespace dynamically at compile time.
-- Copyright   : Copyright 2025 Shea Levy
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- Manage the implicit parameter namespace dynamically at compile time.
--
-- This is an implementation of [GHC proposal 737](https://github.com/ghc-proposals/ghc-proposals/pull/737).
-- Relative to that proposal, this suffers from the following (likely hypothetical) limitations:
--
--   * It may break if future GHC versions change the implementation of implicit parameters
--   * It doesn't mandate that 'bindImplicit' and 'implicitParameter' be fully applied,
--     nor guarantee that they don't impose additional runtime cost.
--   * It uses some auxiliary internal type-level machinery that can't be hidden.
--   * It is not inferred as safe Haskell (though it is safe).
module Data.Implicit where

import GHC.Base (IP, ip, withDict)
import GHC.TypeError
import GHC.TypeLits (Symbol)

-- | 'ImplicitParameter' @x@ @a@ is a t'Data.Kind.Constraint' that 'implicitParameter' @x@ is an implicit parameter of type @a@.
--
-- 'ImplicitParameter' constraints are propagated according to dynamic scoping rules.
-- In other words, GHC always takes the most nested parameter binding from the context
-- to find the value. @bindImplict x a (bindImplict x b (implicitParameter x))@
-- is equivalent to @b@ (including resulting in a type error if used where
-- a term whose type unifies with @a@'s but not @b@'s is expected).
--
-- To bind an implicit parameter, use 'bindImplicit'.
--
-- To unbind an implicit parameter, use 'unbindImplicit'.
--
-- @ImplictParameter (MkIdent "foo") t@ is equivalent to @?foo :: t@.
type ImplicitParameter (x :: k) = IP (KeyName x)

-- | Bind implicit parameter @x@ to the provided @a@.
--
-- @bindImplicit (MkIdent "foo") x go@ is equivalent to @let ?foo = x in go@.
{-# INLINE bindImplicit #-}
bindImplicit :: forall x -> a -> ((ImplicitParameter x a) => b) -> b
bindImplicit _ = withDict

-- | Access implicit parameter bound to @x@.
--
-- @implicitParameter (MkIdent "foo")@ is equivalent to @?foo@.
{-# INLINE implicitParameter #-}
implicitParameter :: forall x -> (ImplicitParameter x a) => a
implicitParameter x = ip @(KeyName x)

-- | Unbind the implicit parameter @x@.
--
-- Unbinding follows the same dynamic scoping rules as 'ImplicitParameter'. @bindImplicit x a (unbindImplicit x (implicitParameter x))@
-- is a compile-time error while @unbindImplicit x (bindImplicit x a (implicitParameter x))@ is equivalent to @a@.
unbindImplicit :: forall x -> ((ImplicitUnbound x) => a) -> a
unbindImplicit key = bindImplicit key undefined

-- | A t'Data.Kind.Constraint' that there is no implicit parameter bound to @x@.
--
-- To unbind an implicit parameter, use 'unbindImplicit'.
type ImplicitUnbound (x :: k) = ImplicitParameter x (TypeError (Text "The implicit parameter " :<>: ShowType x :<>: Text " has been explicitly unbound."))

-- | The kind of type-level representations of Haskell identifiers.
data Ident = MkIdent Symbol {- HLINT ignore "Use newtype instead of data" -}

-- | The identifier's name
type family IdentName (i :: Ident) :: Symbol where
  IdentName (MkIdent s) = s

-- *** Implementation details

-- | Helper data family to create a new 'Symbol' from a type-level value of any kind.
data family MkSymbol (t :: k) :: k'

-- | Turn the key of an 'ImplicitParameter' into a 'Symbol' for GHC's 'IP' type class.
--
-- 'Ident' ifiers are directly mapped to their underlying names.
--
-- Other type-level values are injected into 'Symbol' via 'MkSymbol'.
type KeyName :: forall k. k -> Symbol
type family KeyName t where
  KeyName i = IdentName i
  KeyName t = MkSymbol t
