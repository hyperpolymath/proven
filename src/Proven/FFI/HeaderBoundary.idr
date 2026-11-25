-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
module Proven.FFI.HeaderBoundary

import Data.List
import Data.Nat
import Proven.FFI.HeaderBorrow
import Proven.SafeHeader.ByteKernel

%default total

||| A bounded logical snapshot. Constructing this record does not establish
||| that a raw machine pointer addresses live memory or grants a read lease.
||| Pointer correspondence, usize representability and immutable lifetime are
||| separate premises of the native call, interpreted by Atoma's heap profile.
public export
record Span (memory : List Bits8) where
  constructor MkSpan
  offset : Nat
  count : Nat
  0 inBounds : LTE (offset + count) (length memory)

public export
spanBytes : (memory : List Bits8) -> Span memory -> List Bits8
spanBytes memory (MkSpan offset count _) =
  Data.List.take count (Data.List.drop offset memory)

takeBoundedLength : (count : Nat) -> (memory : List Bits8) ->
  (0 bound : LTE count (length memory)) -> length (Data.List.take count memory) = count
takeBoundedLength Z memory bound = Refl
takeBoundedLength (S count) [] bound impossible
takeBoundedLength (S count) (byte :: rest) (LTESucc bound) =
  cong S (takeBoundedLength count rest bound)

sliceBoundedLength : (offset, count : Nat) -> (memory : List Bits8) ->
  (0 bound : LTE (offset + count) (length memory)) ->
  length (Data.List.take count (Data.List.drop offset memory)) = count
sliceBoundedLength Z count memory bound = takeBoundedLength count memory bound
sliceBoundedLength (S offset) count [] bound impossible
sliceBoundedLength (S offset) count (byte :: rest) (LTESucc bound) =
  sliceBoundedLength offset count rest bound

public export
spanLength : (memory : List Bits8) -> (span : Span memory) ->
  length (spanBytes memory span) = count span
spanLength memory (MkSpan offset count bound) =
  sliceBoundedLength offset count memory bound

public export
data BackendOutcome = Detected Bool | BackendUnavailable | InvalidBackendResult

||| Logical form of the existing public status/Boolean ABI, without claiming
||| a machine layout theorem for Integer or Bool.
public export
record BoundaryResult where
  constructor MkBoundaryResult
  status : Integer
  value : Bool

public export
finish : BackendOutcome -> BoundaryResult
finish (Detected value) = MkBoundaryResult 0 value
finish BackendUnavailable = MkBoundaryResult (-99) False
finish InvalidBackendResult = MkBoundaryResult (-7) False

public export
entry : (memory : List Bits8) -> Maybe (Span memory) ->
  (List Bits8 -> BackendOutcome) -> BoundaryResult
entry memory Nothing backend = MkBoundaryResult (-1) False
entry memory (Just span) backend = finish (backend (spanBytes memory span))

public export
nullRefuses : (memory : List Bits8) -> (backend : List Bits8 -> BackendOutcome) ->
  entry memory Nothing backend = MkBoundaryResult (-1) False
nullRefuses memory backend = Refl

public export
successPreservesBoolean : (value : Bool) ->
  finish (Detected value) = MkBoundaryResult 0 value
successPreservesBoolean value = Refl

public export
unavailableRefuses : finish BackendUnavailable = MkBoundaryResult (-99) False
unavailableRefuses = Refl

public export
invalidRefuses : finish InvalidBackendResult = MkBoundaryResult (-7) False
invalidRefuses = Refl

public export
checkedBackend : List Bits8 -> BackendOutcome
checkedBackend bytes = case checkedScan False bytes of
  Just value => Detected value
  Nothing => InvalidBackendResult

||| Composition uses the authoritative Idris byte fold over exactly the span.
||| The source/compiler and foreign-call correspondence remain external.
public export
entryRefinesFold : (memory : List Bits8) -> (span : Span memory) ->
  entry memory (Just span) Proven.FFI.HeaderBoundary.checkedBackend =
    MkBoundaryResult 0 (scanTyped False (spanBytes memory span))
entryRefinesFold memory span =
  rewrite checkedScanCorrect False (spanBytes memory span) in Refl
