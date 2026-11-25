-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module Proven.SafeHeader.ByteKernel

%default total

public export
isHeaderBreak : Bits8 -> Bool
isHeaderBreak b = b == 10 || b == 13

public export
encodeState : Bool -> Bits8
encodeState False = 0
encodeState True = 1

public export
nextState : Bool -> Bits8 -> Bool
nextState False b = isHeaderBreak b
nextState True _ = True

||| A byte transition. State 0 means no forbidden byte has occurred;
||| state 1 means one has occurred. The ABI starts at 0 and only accepts 0/1.
||| Keep a callable RefC symbol; exporting a module name alone is not C export.
public export
%noinline
headerStep : Bits8 -> Bits8 -> Bits8
headerStep state b = if state == 0 then encodeState (isHeaderBreak b) else state

||| The concrete Bits8 kernel implements the typed Boolean state transition.
public export
stepEncodes : (state : Bool) -> (b : Bits8) ->
  headerStep (encodeState state) b = encodeState (nextState state b)
stepEncodes False b = Refl
stepEncodes True b = Refl

public export
scanTyped : Bool -> List Bits8 -> Bool
scanTyped state [] = state
scanTyped state (b :: bs) = scanTyped (nextState state b) bs

public export
scanBytes : Bits8 -> List Bits8 -> Bits8
scanBytes state [] = state
scanBytes state (b :: bs) = scanBytes (headerStep state b) bs

public export
scanEncodes : (state : Bool) -> (bs : List Bits8) ->
  scanBytes (encodeState state) bs = encodeState (scanTyped state bs)
scanEncodes state [] = Refl
scanEncodes state (b :: bs) = rewrite stepEncodes state b in scanEncodes (nextState state b) bs

public export
seenStaysSeen : (bs : List Bits8) -> scanTyped True bs = True
seenStaysSeen [] = Refl
seenStaysSeen (b :: bs) = seenStaysSeen bs

||| The streaming loop can split anywhere, including around NUL and CR/LF.
public export
scanAppend : (state : Bits8) -> (xs, ys : List Bits8) ->
  scanBytes state (xs ++ ys) = scanBytes (scanBytes state xs) ys
scanAppend state [] ys = Refl
scanAppend state (x :: xs) ys = scanAppend (headerStep state x) xs ys

public export
carriageReturnDetected : headerStep 0 13 = 1
carriageReturnDetected = Refl
public export
lineFeedDetected : headerStep 0 10 = 1
lineFeedDetected = Refl
public export
nulDoesNotTerminate : scanBytes 0 [0, 13] = 1
nulDoesNotTerminate = Refl
public export
highByteIsData : scanBytes 0 [255, 0, 128] = 0
highByteIsData = Refl
