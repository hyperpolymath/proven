-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Randomness interfaces with explicit backend unavailability
|||
||| Type-safe interfaces for cryptographically secure random number generation.
||| No entropy backend is implemented. Requests requiring entropy fail closed.
|||
||| Note: Byte (= Bits8) and Bytes (= List Bits8) are defined in
||| Proven.SafeCrypto to avoid ambiguity when modules are re-exported
||| together. This module uses Bits8 / List Bits8 directly.
module Proven.SafeCrypto.Random

import Proven.Core
import Data.List
import Data.Bits
import Data.Vect
import Decidable.Equality

%default total

--------------------------------------------------------------------------------
-- Basic Types
--------------------------------------------------------------------------------

||| Fixed-size byte vector
public export
data ByteVec : (n : Nat) -> Type where
  MkByteVec : Vect n Bits8 -> ByteVec n

--------------------------------------------------------------------------------
-- Random Generation Result Types
--------------------------------------------------------------------------------

||| Error during random generation
public export
data RandomError
  = InsufficientEntropy        -- Not enough entropy available
  | SystemError String         -- OS-level error
  | GeneratorNotSeeded         -- CSPRNG not properly seeded
  | RequestTooLarge Nat        -- Requested too many bytes
  | EntropyUnavailable          -- No implemented entropy backend
  | InvalidRange Integer Integer
  | CounterExhausted            -- Four-byte counter space exhausted
  | CounterEncodingFailure      -- Encoded counter failed its round-trip check

public export
Show RandomError where
  show InsufficientEntropy = "Insufficient entropy available"
  show (SystemError msg) = "System error: " ++ msg
  show GeneratorNotSeeded = "Random generator not properly seeded"
  show (RequestTooLarge n) = "Request too large: " ++ show n ++ " bytes"
  show EntropyUnavailable = "System entropy backend is unavailable"
  show (InvalidRange lo hi) = "Invalid random range: " ++ show lo ++ " to " ++ show hi
  show CounterExhausted = "Counter exceeds the four-byte nonce domain"
  show CounterEncodingFailure = "Counter nonce failed its round-trip check"

||| Result of random generation
public export
RandomResult : Type -> Type
RandomResult a = Either RandomError a

--------------------------------------------------------------------------------
-- Entropy Source Types
--------------------------------------------------------------------------------

||| Source of randomness
public export
data EntropySource
  = SystemRandom      -- /dev/urandom or CryptGenRandom
  | HardwareRandom    -- RDRAND/RDSEED if available
  | Combined          -- Hardware + system combined

public export
Show EntropySource where
  show SystemRandom = "System CSPRNG"
  show HardwareRandom = "Hardware RNG"
  show Combined = "Combined"

--------------------------------------------------------------------------------
-- Random Generation Interface
--------------------------------------------------------------------------------

||| Generate random bytes
||| No successful result is fabricated while the entropy backend is absent.
public export
randomBytes : (n : Nat) -> RandomResult (ByteVec n)
randomBytes n =
  if n > 1000000  -- Reasonable limit
    then Left (RequestTooLarge n)
    else Left EntropyUnavailable

||| Generate random bytes as list
public export
randomByteList : (n : Nat) -> RandomResult (List Bits8)
randomByteList n = map (\(MkByteVec v) => toList v) (randomBytes n)

||| Generate a random natural number in range [0, max)
public export
randomNat : (max : Nat) -> {auto ok : IsSucc max} -> RandomResult Nat
randomNat max = Left EntropyUnavailable

||| Generate a random natural number in range [min, max]
public export
randomNatRange : (min : Nat) -> (max : Nat) ->
                 {auto ok : LTE min max} ->
                 RandomResult Nat
randomNatRange min max = map (\n => min + n) (randomNat (S (minus max min)))

||| Generate a random integer in range
public export
randomInt : (min : Integer) -> (max : Integer) -> RandomResult Integer
randomInt min max =
  if min > max
    then Left (InvalidRange min max)
    else Left EntropyUnavailable

||| Generate a random boolean
public export
randomBool : RandomResult Bool
randomBool = map (\n => n == 0) (randomNat 2)

||| Generate a random double in [0, 1)
public export
randomDouble : RandomResult Double
randomDouble = Left EntropyUnavailable

--------------------------------------------------------------------------------
-- Secure Token Generation
--------------------------------------------------------------------------------

||| Generate a secure random token (URL-safe base64)
public export
randomToken : (bytes : Nat) -> RandomResult String
randomToken n = map toBase64 (randomByteList n)
  where
    -- URL-safe base64 alphabet
    alphabet : Vect 64 Char
    alphabet = the (Vect 64 Char)
      [ 'A','B','C','D','E','F','G','H','I','J','K','L','M'
      , 'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'
      , 'a','b','c','d','e','f','g','h','i','j','k','l','m'
      , 'n','o','p','q','r','s','t','u','v','w','x','y','z'
      , '0','1','2','3','4','5','6','7','8','9','-','_'
      ]

    toBase64 : List Bits8 -> String
    toBase64 bytes = pack (go bytes [])
      where
        getChar : Nat -> Char
        getChar n = index (restrict 63 (cast n)) alphabet

        go : List Bits8 -> List Char -> List Char
        go [] acc = reverse acc
        go [b1] acc =
          let c1 = getChar (cast (shiftR b1 2))
              c2 = getChar (cast ((shiftL b1 4) .&. 0x3F))
          in reverse (c2 :: c1 :: acc)
        go [b1, b2] acc =
          let c1 = getChar (cast (shiftR b1 2))
              c2 = getChar (cast (((shiftL b1 4) .|. (shiftR b2 4)) .&. 0x3F))
              c3 = getChar (cast ((shiftL b2 2) .&. 0x3F))
          in reverse (c3 :: c2 :: c1 :: acc)
        go (b1 :: b2 :: b3 :: rest) acc =
          let c1 = getChar (cast (shiftR b1 2))
              c2 = getChar (cast (((shiftL b1 4) .|. (shiftR b2 4)) .&. 0x3F))
              c3 = getChar (cast (((shiftL b2 2) .|. (shiftR b3 6)) .&. 0x3F))
              c4 = getChar (cast (b3 .&. 0x3F))
          in go rest (c4 :: c3 :: c2 :: c1 :: acc)

||| Generate a random hex string
public export
randomHex : (bytes : Nat) -> RandomResult String
randomHex n = map bytesToHex (randomByteList n)
  where
    bytesToHex : List Bits8 -> String
    bytesToHex bs = concat (map byteToHex bs)
      where
        hexDigit : Nat -> Char
        hexDigit d = if d < 10 then chr (ord '0' + cast d) else chr (ord 'a' + cast d - 10)

        byteToHex : Bits8 -> String
        byteToHex b =
          let n = cast {to=Nat} b
          in pack [hexDigit (n `div` 16), hexDigit (n `mod` 16)]

||| Generate a UUID v4 (random)
public export
randomUUID : RandomResult String
randomUUID = do
  bytes <- randomByteList 16
  case bytes of
    [b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15] =>
      let -- Set version (4) and variant (10xx)
          b6' = (b6 .&. 0x0F) .|. 0x40  -- Version 4
          b8' = (b8 .&. 0x3F) .|. 0x80  -- Variant 10xx
          bytes' = [b0,b1,b2,b3,b4,b5,b6',b7,b8',b9,b10,b11,b12,b13,b14,b15]
      in Right (formatUUID bytes')
    _ => Left (SystemError "Failed to generate 16 bytes")
  where
    formatUUID : List Bits8 -> String
    formatUUID bs = case bs of
      [b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15] =>
        hexByte b0 ++ hexByte b1 ++ hexByte b2 ++ hexByte b3 ++ "-" ++
        hexByte b4 ++ hexByte b5 ++ "-" ++
        hexByte b6 ++ hexByte b7 ++ "-" ++
        hexByte b8 ++ hexByte b9 ++ "-" ++
        hexByte b10 ++ hexByte b11 ++ hexByte b12 ++ hexByte b13 ++ hexByte b14 ++ hexByte b15
      _ => ""
      where
        hexDigit : Nat -> Char
        hexDigit d = if d < 10 then chr (ord '0' + cast d) else chr (ord 'a' + cast d - 10)

        hexByte : Bits8 -> String
        hexByte b =
          let n = cast {to=Nat} b
          in pack [hexDigit (n `div` 16), hexDigit (n `mod` 16)]

--------------------------------------------------------------------------------
-- Shuffling and Selection
--------------------------------------------------------------------------------

||| Empty/singleton shuffles are deterministic. Larger lists require entropy.
public export
shuffle : List a -> RandomResult (List a)
shuffle [] = Right []
shuffle [x] = Right [x]
shuffle (_ :: _ :: _) = Left EntropyUnavailable
||| Pick n random elements from a list
public export
sample : (n : Nat) -> List a -> RandomResult (List a)
sample Z xs = Right []
sample n xs =
  if n >= length xs
    then Right xs
    else map (take n) (shuffle xs)

||| Pick one random element from a non-empty list
public export
choice : (xs : List a) -> {auto ok : NonEmpty xs} -> RandomResult a
choice (x :: xs) = do
  idx <- randomNat (S (length xs))
  Right (index' idx (x :: xs))
  where
    index' : Nat -> List a -> a
    index' _ [] = x  -- Fallback (shouldn't happen)
    index' Z (y :: _) = y
    index' (S n) (_ :: ys) = index' n ys

--------------------------------------------------------------------------------
-- Nonce Generation
--------------------------------------------------------------------------------

||| Generate a fresh nonce of specified size
public export
freshNonce : (n : Nat) -> RandomResult (ByteVec n)
freshNonce = randomBytes

||| Read the four-byte big-endian counter from a twelve-byte nonce.
public export
nonceCounter : ByteVec 12 -> Bits64
nonceCounter (MkByteVec [_, _, _, _, _, _, _, _, b3, b2, b1, b0]) =
  shiftL (cast b3) 24 .|. shiftL (cast b2) 16 .|. shiftL (cast b1) 8 .|. cast b0

encodeCounterNonce : ByteVec 8 -> Bits64 -> ByteVec 12
encodeCounterNonce (MkByteVec pfxBytes) counter =
  let b3 : Bits8 = cast (shiftR counter 24)
      b2 : Bits8 = cast (shiftR counter 16)
      b1 : Bits8 = cast (shiftR counter 8)
      b0 : Bits8 = cast counter
  in MkByteVec (pfxBytes ++ [b3, b2, b1, b0])

||| The round-trip check is part of production, not an assumed bit-shift law.
checkCounterEncoding : Bits64 -> ByteVec 12 -> RandomResult (ByteVec 12)
checkCounterEncoding counter nonce = case decEq (nonceCounter nonce) counter of
  Yes _ => Right nonce
  No _ => Left CounterEncodingFailure

||| Fixed 96-bit format: eight-byte prefix and four-byte counter.
||| Reject exhaustion instead of silently truncating a Bits64 counter.
||| Caller must persist counters, never reuse them under a key/prefix, and rotate
||| the key/prefix on exhaustion. This pure function does not manage that state.
public export
counterNonce : (pfx : ByteVec 8) -> (counter : Bits64) -> RandomResult (ByteVec 12)
counterNonce pfx counter =
  if counter > 4294967295
    then Left CounterExhausted
    else checkCounterEncoding counter (encodeCounterNonce pfx counter)

checkedEncodingRoundTrip : (counter : Bits64) -> (nonce, output : ByteVec 12) ->
  checkCounterEncoding counter nonce = Right output -> nonceCounter output = counter
checkedEncodingRoundTrip counter nonce output accepted with (decEq (nonceCounter nonce) counter)
  checkedEncodingRoundTrip counter nonce nonce Refl | Yes evidence = evidence
  checkedEncodingRoundTrip counter nonce output Refl | No _ impossible

||| Every accepted production result decodes to the ORIGINAL counter.
public export
counterNonceRoundTrip : (pfx : ByteVec 8) -> (counter : Bits64) ->
  (nonce : ByteVec 12) -> counterNonce pfx counter = Right nonce ->
  nonceCounter nonce = counter
counterNonceRoundTrip pfx counter nonce accepted with (counter > 4294967295)
  counterNonceRoundTrip pfx counter nonce Refl | True impossible
  counterNonceRoundTrip pfx counter nonce accepted | False =
    checkedEncodingRoundTrip counter (encodeCounterNonce pfx counter) nonce accepted

||| No two distinct counters can have the same successful nonce result.
public export
counterNonceInjective : (pfx : ByteVec 8) -> (c1, c2 : Bits64) ->
  (n1, n2 : ByteVec 12) -> counterNonce pfx c1 = Right n1 ->
  counterNonce pfx c2 = Right n2 -> n1 = n2 -> c1 = c2
counterNonceInjective pfx c1 c2 n1 n2 accepted1 accepted2 same =
  trans (sym (counterNonceRoundTrip pfx c1 n1 accepted1))
        (trans (cong nonceCounter same) (counterNonceRoundTrip pfx c2 n2 accepted2))

||| Exhausted input is rejected before encoding.
public export
counterNonceExhausted : (pfx : ByteVec 8) -> (counter : Bits64) ->
  (counter > 4294967295) = True -> counterNonce pfx counter = Left CounterExhausted
counterNonceExhausted pfx counter exhausted with (counter > 4294967295)
  counterNonceExhausted pfx counter exhausted | True = Refl
  counterNonceExhausted pfx counter Refl | False impossible

||| Positive endpoint witnesses: the checked API does not reject every input.
public export
zeroCounterAccepted : (bytes : Vect 8 Bits8) ->
  counterNonce (MkByteVec bytes) 0 = Right (MkByteVec (bytes ++ [0,0,0,0]))
zeroCounterAccepted [a,b,c,d,e,f,g,h] = Refl

public export
lastCounterAccepted : (bytes : Vect 8 Bits8) ->
  counterNonce (MkByteVec bytes) 4294967295 = Right (MkByteVec (bytes ++ [255,255,255,255]))
lastCounterAccepted [a,b,c,d,e,f,g,h] = Refl

--------------------------------------------------------------------------------
-- Seed Generation
--------------------------------------------------------------------------------

||| Generate a seed for deterministic PRNG
public export
generateSeed : (n : Nat) -> RandomResult (ByteVec n)
generateSeed = randomBytes

||| Mix additional entropy into seed
public export
mixEntropy : ByteVec n -> List Bits8 -> ByteVec n
mixEntropy (MkByteVec seed) extra =
  MkByteVec (mixVect seed extra)
  where
    ||| XOR each seed byte with the corresponding extra byte (0-padded)
    mixVect : Vect m Bits8 -> List Bits8 -> Vect m Bits8
    mixVect []        _         = []
    mixVect (s :: ss) []        = s :: mixVect ss []
    mixVect (s :: ss) (e :: es) = (s `xor` e) :: mixVect ss es
