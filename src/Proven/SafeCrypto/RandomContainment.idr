-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module Proven.SafeCrypto.RandomContainment

import Proven.SafeCrypto.Random
import Data.List
import Data.Nat

%default total

||| Rejection proofs, not entropy quality or cryptographic security proofs.
public export
randomBytesUnavailable : (n : Nat) -> (bytes : ByteVec n) ->
  randomBytes n = Right bytes -> Void
randomBytesUnavailable n bytes accepted with (n > 1000000)
  randomBytesUnavailable n bytes Refl | True impossible
  randomBytesUnavailable n bytes Refl | False impossible

public export
randomNatUnavailable : (bound : Nat) -> {auto ok : IsSucc bound} ->
  (n : Nat) -> randomNat bound = Right n -> Void
randomNatUnavailable bound n Refl impossible

public export
randomIntUnavailable : (lo, hi, output : Integer) ->
  randomInt lo hi = Right output -> Void
randomIntUnavailable lo hi output accepted with (lo > hi)
  randomIntUnavailable lo hi output Refl | True impossible
  randomIntUnavailable lo hi output Refl | False impossible

public export
randomDoubleUnavailable : (output : Double) -> Proven.SafeCrypto.Random.randomDouble = Right output -> Void
randomDoubleUnavailable output Refl impossible

public export
randomTokenUnavailable : (n : Nat) -> (output : String) ->
  randomToken n = Right output -> Void
randomTokenUnavailable n output accepted with (n > 1000000)
  randomTokenUnavailable n output Refl | True impossible
  randomTokenUnavailable n output Refl | False impossible

public export
randomUUIDUnavailable : (output : String) -> Proven.SafeCrypto.Random.randomUUID = Right output -> Void
randomUUIDUnavailable output Refl impossible

public export
freshNonceUnavailable : (n : Nat) -> (bytes : ByteVec n) ->
  freshNonce n = Right bytes -> Void
freshNonceUnavailable = randomBytesUnavailable

public export
generateSeedUnavailable : (n : Nat) -> (bytes : ByteVec n) ->
  generateSeed n = Right bytes -> Void
generateSeedUnavailable = randomBytesUnavailable

public export
nontrivialShuffleUnavailable : (x, y : a) -> (rest, output : List a) ->
  shuffle (x :: y :: rest) = Right output -> Void
nontrivialShuffleUnavailable x y rest output Refl impossible

||| Empty and singleton shuffles need no random choice.
public export
emptyShuffle : shuffle {a} [] = Right []
emptyShuffle = Refl

public export
singletonShuffle : (x : a) -> shuffle [x] = Right [x]
singletonShuffle x = Refl

public export
emptySample : (xs : List a) -> sample 0 xs = Right []
emptySample xs = Refl

