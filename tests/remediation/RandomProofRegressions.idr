-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module RandomProofRegressions
import Proven.SafeCrypto.Random
import Proven.SafeCrypto.RandomContainment
import Proven.SafeCrypto.Proofs
import Data.Vect
%default total

acceptedRoundTrip : (p : ByteVec 8) -> (c : Bits64) -> (n : ByteVec 12) ->
  counterNonce p c = Right n -> nonceCounter n = c
acceptedRoundTrip = counterNonceRoundTrip

acceptedInjectivity : (p : ByteVec 8) -> (a, b : Bits64) -> (x, y : ByteVec 12) ->
  counterNonce p a = Right x -> counterNonce p b = Right y -> x = y -> a = b
acceptedInjectivity = counterNonceInjective

acceptedDistinct : (p : ByteVec 8) -> (a, b : Bits64) -> (x, y : ByteVec 12) ->
  counterNonce p a = Right x -> counterNonce p b = Right y -> Not (a = b) -> Not (x = y)
acceptedDistinct = counterNonceUnique

exhaustionRejected : (p : ByteVec 8) -> (c : Bits64) -> (c > 4294967295) = True ->
  counterNonce p c = Left CounterExhausted
exhaustionRejected = counterNonceExhausted

zeroAccepted : counterNonce (MkByteVec [0,0,0,0,0,0,0,0]) 0 =
  Right (MkByteVec [0,0,0,0,0,0,0,0,0,0,0,0])
zeroAccepted = zeroCounterAccepted [0,0,0,0,0,0,0,0]

lastAccepted : counterNonce (MkByteVec [1,2,3,4,5,6,7,8]) 4294967295 =
  Right (MkByteVec [1,2,3,4,5,6,7,8,255,255,255,255])
lastAccepted = lastCounterAccepted [1,2,3,4,5,6,7,8]

overflowRejected : counterNonce (MkByteVec [0,0,0,0,0,0,0,0]) 4294967296 =
  Left CounterExhausted
overflowRejected = Refl

noInventedEntropy : (n : Nat) -> (bytes : ByteVec n) ->
  randomBytes n = Right bytes -> Void
noInventedEntropy = randomBytesUnavailable
