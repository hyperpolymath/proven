-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Result type and utilities for the Proven library.
-- |
-- | Provides a comprehensive error type and Result monad for safe operations.

module Proven.Result
  ( Result(..)
  , ProvenError(..)
  , ok
  , err
  , isOk
  , isErr
  , unwrap
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

-- | Comprehensive error types for proven operations.
-- | Covers all safety-critical failure modes across the library.
data ProvenError
  = Overflow
  | Underflow
  | DivisionByZero
  | OutOfBounds
  | InvalidPort
  | InvalidPercentage
  | InvalidEmail
  | InvalidUrl
  | PathTraversal
  | InvalidFormat String
  | InvalidInput String
  | InvalidUtf8 String
  | InvalidState String
  | InvalidTransition String
  | InvalidChecksum
  | InvalidVersion
  | InvalidColor
  | InvalidCoordinate
  | InvalidAngle
  | InvalidUuid
  | InvalidCurrency
  | InvalidPhone
  | InvalidHex
  | InvalidHeader
  | InvalidCookie
  | InvalidContentType
  | InvalidJson String
  | InvalidDateTime String
  | TooLong String
  | TooShort String
  | BufferFull
  | BufferEmpty
  | QueueFull
  | QueueEmpty
  | RateLimited Int
  | CircuitOpen
  | RetryExhausted
  | HashCollision
  | GraphCycle
  | NodeNotFound String
  | EdgeNotFound String
  | PasswordTooWeak String
  | TensorShapeMismatch
  | ProbabilityOutOfRange
  | InjectionDetected
  | CrlfInjection
  | Custom String

derive instance eqProvenError :: Eq ProvenError

instance showProvenError :: Show ProvenError where
  show Overflow = "Overflow"
  show Underflow = "Underflow"
  show DivisionByZero = "DivisionByZero"
  show OutOfBounds = "OutOfBounds"
  show InvalidPort = "InvalidPort"
  show InvalidPercentage = "InvalidPercentage"
  show InvalidEmail = "InvalidEmail"
  show InvalidUrl = "InvalidUrl"
  show PathTraversal = "PathTraversal"
  show (InvalidFormat s) = "InvalidFormat: " <> s
  show (InvalidInput s) = "InvalidInput: " <> s
  show (InvalidUtf8 s) = "InvalidUtf8: " <> s
  show (InvalidState s) = "InvalidState: " <> s
  show (InvalidTransition s) = "InvalidTransition: " <> s
  show InvalidChecksum = "InvalidChecksum"
  show InvalidVersion = "InvalidVersion"
  show InvalidColor = "InvalidColor"
  show InvalidCoordinate = "InvalidCoordinate"
  show InvalidAngle = "InvalidAngle"
  show InvalidUuid = "InvalidUuid"
  show InvalidCurrency = "InvalidCurrency"
  show InvalidPhone = "InvalidPhone"
  show InvalidHex = "InvalidHex"
  show InvalidHeader = "InvalidHeader"
  show InvalidCookie = "InvalidCookie"
  show InvalidContentType = "InvalidContentType"
  show (InvalidJson s) = "InvalidJson: " <> s
  show (InvalidDateTime s) = "InvalidDateTime: " <> s
  show (TooLong s) = "TooLong: " <> s
  show (TooShort s) = "TooShort: " <> s
  show BufferFull = "BufferFull"
  show BufferEmpty = "BufferEmpty"
  show QueueFull = "QueueFull"
  show QueueEmpty = "QueueEmpty"
  show (RateLimited n) = "RateLimited: retry after " <> show n
  show CircuitOpen = "CircuitOpen"
  show RetryExhausted = "RetryExhausted"
  show HashCollision = "HashCollision"
  show GraphCycle = "GraphCycle"
  show (NodeNotFound s) = "NodeNotFound: " <> s
  show (EdgeNotFound s) = "EdgeNotFound: " <> s
  show (PasswordTooWeak s) = "PasswordTooWeak: " <> s
  show TensorShapeMismatch = "TensorShapeMismatch"
  show ProbabilityOutOfRange = "ProbabilityOutOfRange"
  show InjectionDetected = "InjectionDetected"
  show CrlfInjection = "CrlfInjection"
  show (Custom s) = "Custom: " <> s

-- | Result type for operations that can fail.
-- | A pure functional alternative to exceptions.
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

-- | Create a success result.
ok :: forall a e. a -> Result a e
ok = Ok

-- | Create an error result.
err :: forall a e. e -> Result a e
err = Err

-- | Check if result is ok.
isOk :: forall a e. Result a e -> Boolean
isOk (Ok _) = true
isOk (Err _) = false

-- | Check if result is error.
isErr :: forall a e. Result a e -> Boolean
isErr = not <<< isOk

-- | Unwrap result or throw error message.
unwrap :: forall a. Result a ProvenError -> a
unwrap (Ok a) = a
unwrap (Err e) = unsafeThrow (show e)

foreign import unsafeThrow :: forall a. String -> a

-- | Unwrap result with default value.
unwrapOr :: forall a e. a -> Result a e -> a
unwrapOr _ (Ok a) = a
unwrapOr def (Err _) = def

-- | Map over successful result.
mapResult :: forall a b e. (a -> b) -> Result a e -> Result b e
mapResult f (Ok a) = Ok (f a)
mapResult _ (Err e) = Err e

-- | FlatMap over successful result.
flatMapResult :: forall a b e. (a -> Result b e) -> Result a e -> Result b e
flatMapResult f (Ok a) = f a
flatMapResult _ (Err e) = Err e

-- | Convert Maybe to Result.
fromMaybe :: forall a e. e -> Maybe a -> Result a e
fromMaybe _ (Just a) = Ok a
fromMaybe e Nothing = Err e

-- | Convert Result to Maybe.
toMaybe :: forall a e. Result a e -> Maybe a
toMaybe (Ok a) = Just a
toMaybe (Err _) = Nothing

-- | Convert Either to Result.
fromEither :: forall a e. Either e a -> Result a e
fromEither (Right a) = Ok a
fromEither (Left e) = Err e

-- | Convert Result to Either.
toEither :: forall a e. Result a e -> Either e a
toEither (Ok a) = Right a
toEither (Err e) = Left e
