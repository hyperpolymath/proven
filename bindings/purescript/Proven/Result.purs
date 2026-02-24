-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | Result type for the Proven FFI library.
-- |
-- | Provides error types matching libproven status codes and a Result monad.

module Proven.Result
  ( Result(..)
  , ProvenError(..)
  , ok
  , err
  , isOk
  , isErr
  , unwrapOr
  , mapResult
  , flatMapResult
  , fromMaybe
  , toMaybe
  , fromEither
  , toEither
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | Error types matching libproven ProvenStatus codes
data ProvenError
  = Overflow
  | Underflow
  | DivisionByZero
  | OutOfBounds
  | InvalidFormat String
  | InvalidInput String
  | InvalidEmail
  | InvalidUrl
  | PathTraversal
  | InvalidUuid
  | InvalidCurrency
  | InvalidPhone
  | InvalidHex
  | InvalidJson String
  | InvalidDateTime String
  | InvalidColor
  | InvalidAngle
  | InvalidVersion
  | ParseFailure
  | ValidationFailed
  | NullPointer
  | EncodingError
  | NotFound
  | Custom String

derive instance eqProvenError :: Eq ProvenError

instance showProvenError :: Show ProvenError where
  show Overflow = "Overflow"
  show Underflow = "Underflow"
  show DivisionByZero = "DivisionByZero"
  show OutOfBounds = "OutOfBounds"
  show (InvalidFormat s) = "InvalidFormat: " <> s
  show (InvalidInput s) = "InvalidInput: " <> s
  show InvalidEmail = "InvalidEmail"
  show InvalidUrl = "InvalidUrl"
  show PathTraversal = "PathTraversal"
  show InvalidUuid = "InvalidUuid"
  show InvalidCurrency = "InvalidCurrency"
  show InvalidPhone = "InvalidPhone"
  show InvalidHex = "InvalidHex"
  show (InvalidJson s) = "InvalidJson: " <> s
  show (InvalidDateTime s) = "InvalidDateTime: " <> s
  show InvalidColor = "InvalidColor"
  show InvalidAngle = "InvalidAngle"
  show InvalidVersion = "InvalidVersion"
  show ParseFailure = "ParseFailure"
  show ValidationFailed = "ValidationFailed"
  show NullPointer = "NullPointer"
  show EncodingError = "EncodingError"
  show NotFound = "NotFound"
  show (Custom s) = "Custom: " <> s

-- | Result type for operations that can fail
data Result a e
  = Ok a
  | Err e

derive instance eqResult :: (Eq a, Eq e) => Eq (Result a e)

instance showResult :: (Show a, Show e) => Show (Result a e) where
  show (Ok a) = "Ok(" <> show a <> ")"
  show (Err e) = "Err(" <> show e <> ")"

instance functorResult :: Functor (Result a) where
  map _ (Err e) = Err e
  map f (Ok a) = Ok (f a)

instance applyResult :: Apply (Result e) where
  apply (Ok f) (Ok a) = Ok (f a)
  apply (Err e) _ = Err e
  apply _ (Err e) = Err e

instance applicativeResult :: Applicative (Result e) where
  pure = Ok

instance bindResult :: Bind (Result e) where
  bind (Ok a) f = f a
  bind (Err e) _ = Err e

instance monadResult :: Monad (Result e)

ok :: forall a e. a -> Result a e
ok = Ok

err :: forall a e. e -> Result a e
err = Err

isOk :: forall a e. Result a e -> Boolean
isOk (Ok _) = true
isOk (Err _) = false

isErr :: forall a e. Result a e -> Boolean
isErr = not <<< isOk

unwrapOr :: forall a e. a -> Result a e -> a
unwrapOr _ (Ok a) = a
unwrapOr def (Err _) = def

mapResult :: forall a b e. (a -> b) -> Result a e -> Result b e
mapResult f (Ok a) = Ok (f a)
mapResult _ (Err e) = Err e

flatMapResult :: forall a b e. (a -> Result b e) -> Result a e -> Result b e
flatMapResult f (Ok a) = f a
flatMapResult _ (Err e) = Err e

fromMaybe :: forall a e. e -> Maybe a -> Result a e
fromMaybe _ (Just a) = Ok a
fromMaybe e Nothing = Err e

toMaybe :: forall a e. Result a e -> Maybe a
toMaybe (Ok a) = Just a
toMaybe (Err _) = Nothing

fromEither :: forall a e. Either e a -> Result a e
fromEither (Right a) = Ok a
fromEither (Left e) = Err e

toEither :: forall a e. Result a e -> Either e a
toEither (Ok a) = Right a
toEither (Err e) = Left e
