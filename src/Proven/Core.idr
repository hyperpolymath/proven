-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Core types and utilities for Proven
|||
||| This module provides the foundational types used throughout
||| the library, including error handling and result types.
module Proven.Core
import Data.String
import Data.List

import public Data.List
import public Data.Nat
import public Data.So
import public Data.String

%default total

--------------------------------------------------------------------------------
-- Result Types
--------------------------------------------------------------------------------

||| A result that can either succeed with a value or fail with an error.
||| Unlike exceptions, this forces explicit handling of failure cases.
public export
data Result : Type -> Type -> Type where
  ||| Operation succeeded with a value
  Ok : (value : a) -> Result e a
  ||| Operation failed with an error
  Err : (error : e) -> Result e a

||| Functor instance for Result
public export
Functor (Result e) where
  map f (Ok x) = Ok (f x)
  map f (Err e) = Err e

||| Applicative instance for Result
public export
Applicative (Result e) where
  pure = Ok
  (Ok f) <*> (Ok x) = Ok (f x)
  (Err e) <*> _ = Err e
  _ <*> (Err e) = Err e

||| Monad instance for Result
public export
Monad (Result e) where
  (Ok x) >>= f = f x
  (Err e) >>= _ = Err e

||| Check if a Result is Ok
public export
isOk : Result e a -> Bool
isOk (Ok _) = True
isOk (Err _) = False

||| Check if a Result is an error
public export
isErr : Result e a -> Bool
isErr = not . isOk

||| Extract the value from a Result, with a default
public export
fromResult : a -> Result e a -> a
fromResult _ (Ok x) = x
fromResult def (Err _) = def

||| Map over the error type
public export
mapErr : (e1 -> e2) -> Result e1 a -> Result e2 a
mapErr _ (Ok x) = Ok x
mapErr f (Err e) = Err (f e)

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

||| Common error type for string-based errors
public export
data SafeError : Type where
  ||| Generic error with a message
  MkError : (message : String) -> SafeError
  ||| Error with a code and message
  CodedError : (code : Nat) -> (message : String) -> SafeError
  ||| Validation error with field name
  ValidationError : (field : String) -> (message : String) -> SafeError

public export
Show SafeError where
  show (MkError msg) = "Error: " ++ msg
  show (CodedError code msg) = "Error " ++ show code ++ ": " ++ msg
  show (ValidationError field msg) = "Validation error on '" ++ field ++ "': " ++ msg

public export
Eq SafeError where
  (MkError m1) == (MkError m2) = m1 == m2
  (CodedError c1 m1) == (CodedError c2 m2) = c1 == c2 && m1 == m2
  (ValidationError f1 m1) == (ValidationError f2 m2) = f1 == f2 && m1 == m2
  _ == _ = False

--------------------------------------------------------------------------------
-- Option Type Utilities
--------------------------------------------------------------------------------

||| Safe way to get from a Maybe with a default
public export
withDefault : a -> Maybe a -> a
withDefault def Nothing = def
withDefault _ (Just x) = x

||| Convert Maybe to Result
public export
maybeToResult : e -> Maybe a -> Result e a
maybeToResult _ (Just x) = Ok x
maybeToResult e Nothing = Err e

||| Convert Result to Maybe (discarding error)
public export
resultToMaybe : Result e a -> Maybe a
resultToMaybe (Ok x) = Just x
resultToMaybe (Err _) = Nothing

--------------------------------------------------------------------------------
-- Bounded Integers
--------------------------------------------------------------------------------

||| A bounded integer type with compile-time bounds checking
||| @lo Lower bound (inclusive)
||| @hi Upper bound (inclusive)
public export
data Bounded : (lo : Integer) -> (hi : Integer) -> Type where
  MkBounded : (value : Integer) ->
              {auto loOk : So (lo <= value)} ->
              {auto hiOk : So (value <= hi)} ->
              Bounded lo hi

||| Extract the value from a bounded integer
public export
boundedValue : Bounded lo hi -> Integer
boundedValue (MkBounded v) = v

||| Attempt to create a bounded integer, returning Nothing if out of bounds
public export
mkBounded : (lo : Integer) -> (hi : Integer) -> (value : Integer) -> Maybe (Bounded lo hi)
mkBounded lo hi value with (choose (lo <= value), choose (value <= hi))
  mkBounded lo hi value | (Left loOk, Left hiOk) = Just (MkBounded value)
  mkBounded lo hi value | _ = Nothing

--------------------------------------------------------------------------------
-- NonEmpty Types
--------------------------------------------------------------------------------

||| A non-empty list - guaranteed to have at least one element
public export
data NonEmptyList : Type -> Type where
  MkNonEmptyList : (head : a) -> (tail : List a) -> NonEmptyList a

||| Get the head of a non-empty list (always succeeds)
public export
neHead : NonEmptyList a -> a
neHead (MkNonEmptyList h _) = h

||| Get the tail of a non-empty list
public export
neTail : NonEmptyList a -> List a
neTail (MkNonEmptyList _ t) = t

||| Convert to a regular list
public export
neToList : NonEmptyList a -> List a
neToList (MkNonEmptyList h t) = h :: t

||| Attempt to create a NonEmpty from a list
public export
fromList : List a -> Maybe (NonEmptyList a)
fromList [] = Nothing
fromList (x :: xs) = Just (MkNonEmptyList x xs)

||| Length of a non-empty list (always >= 1)
public export
neLength : NonEmptyList a -> Nat
neLength (MkNonEmptyList _ t) = S (length t)

--------------------------------------------------------------------------------
-- Positive Natural Numbers
--------------------------------------------------------------------------------

||| A positive natural number (> 0)
public export
data Positive : Type where
  MkPositive : (n : Nat) -> {auto ok : IsSucc n} -> Positive

||| Extract the Nat from a Positive
public export
positiveToNat : Positive -> Nat
positiveToNat (MkPositive n) = n

||| Create a Positive from a Nat, returning Nothing for 0
public export
toPositive : Nat -> Maybe Positive
toPositive Z = Nothing
toPositive (S n) = Just (MkPositive (S n))

--------------------------------------------------------------------------------
-- Proof Utilities
--------------------------------------------------------------------------------

||| Evidence that a proposition is decidable
public export
data Decision : Type -> Type where
  Yes : (prf : a) -> Decision a
  No : (contra : a -> Void) -> Decision a

||| A proof that something is impossible (leads to contradiction)
public export
absurd : Void -> a
absurd v impossible
