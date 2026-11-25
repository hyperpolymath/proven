-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
module Proven.FFI.HeaderBorrow
import Proven.SafeHeader.ByteKernel

%default total

||| Contract of the checked state conversion used by the Zig borrowed scan.
||| The actual RefC word decoder has a separate source-bound proof.
public export
acceptState : Bits8 -> Maybe Bool
acceptState value = if value == 0 then Just False else
                   if value == 1 then Just True else Nothing

public export
acceptEncoded : (state : Bool) -> acceptState (encodeState state) = Just state
acceptEncoded False = Refl
acceptEncoded True = Refl

public export
checkedScan : Bool -> List Bits8 -> Maybe Bool
checkedScan state [] = Just state
checkedScan state (b :: bs) = case acceptState (headerStep (encodeState state) b) of
  Nothing => Nothing
  Just next => checkedScan next bs

public export
checkedScanCorrect : (state : Bool) -> (bytes : List Bits8) ->
  checkedScan state bytes = Just (scanTyped state bytes)
checkedScanCorrect state [] = Refl
checkedScanCorrect state (b :: bs) =
  rewrite stepEncodes state b in
  rewrite acceptEncoded (nextState state b) in
  checkedScanCorrect (nextState state b) bs

||| Readability, immutable lifetime and non-aliasing with foreign writes are
||| premises of the machine span. This pure list theorem does not prove them.
public export
borrowedPrefix : Nat -> List Bits8 -> Maybe Bool
borrowedPrefix count bytes = checkedScan False (take count bytes)
