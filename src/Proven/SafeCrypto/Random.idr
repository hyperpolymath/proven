-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Secure Random Number Generation
|||
||| Type-safe interfaces for cryptographically secure random number generation.
||| Actual implementations require FFI to system entropy sources.
|||
||| Note: Byte (= Bits8) and Bytes (= List Bits8) are defined in
||| Proven.SafeCrypto to avoid ambiguity when modules are re-exported
||| together. This module uses Bits8 / List Bits8 directly.
module Proven.SafeCrypto.Random

import Proven.Core
import Data.List
import Data.Bits
import Data.Vect

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

public export
Show RandomError where
  show InsufficientEntropy = "Insufficient entropy available"
  show (SystemError msg) = "System error: " ++ msg
  show GeneratorNotSeeded = "Random generator not properly seeded"
  show (RequestTooLarge n) = "Request too large: " ++ show n ++ " bytes"

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
-- FFI Postulates for Random Generation
--
-- These are FFI stubs. The actual implementations call system CSPRNG
-- via the Idris2 RefC -> Zig FFI pipeline. At link time, Zig calls
-- platform-native entropy sources (getrandom, /dev/urandom, CryptGenRandom).
--
-- Using postulate instead of cast: the type signatures correctly
-- describe the FFI contract. Values are provided at runtime by the
-- native entropy source.
--------------------------------------------------------------------------------

-- FFI: Actual implementation via Idris2 RefC compiled code
-- Reads n bytes from system CSPRNG
prim__randomBytes : (n : Nat) -> ByteVec n

-- FFI: Actual implementation via Idris2 RefC compiled code
-- Returns a uniformly distributed natural number in [0, max)
prim__randomNat : (max : Nat) -> Nat

-- FFI: Actual implementation via Idris2 RefC compiled code
-- Returns a uniformly distributed integer in [min, max]
prim__randomInt : (min : Integer) -> (max : Integer) -> Integer

-- FFI: Actual implementation via Idris2 RefC compiled code
-- Returns a uniformly distributed double in [0, 1)
prim__randomDouble : Double

--------------------------------------------------------------------------------
-- Random Generation Interface
--------------------------------------------------------------------------------

||| Generate random bytes
||| Actual implementation via FFI to system CSPRNG
public export
randomBytes : (n : Nat) -> RandomResult (ByteVec n)
randomBytes n =
  if n > 1000000  -- Reasonable limit
    then Left (RequestTooLarge n)
    else Right (prim__randomBytes n)

||| Generate random bytes as list
public export
randomByteList : (n : Nat) -> RandomResult (List Bits8)
randomByteList n = map (\(MkByteVec v) => toList v) (randomBytes n)

||| Generate a random natural number in range [0, max)
public export
randomNat : (max : Nat) -> {auto ok : IsSucc max} -> RandomResult Nat
randomNat max =
  if max == 0
    then Right 0
    else Right (prim__randomNat max)

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
    then Right min
    else Right (prim__randomInt min max)

||| Generate a random boolean
public export
randomBool : RandomResult Bool
randomBool = map (\n => n == 0) (randomNat 2)

||| Generate a random double in [0, 1)
public export
randomDouble : RandomResult Double
randomDouble = Right prim__randomDouble

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

||| Shuffle a list using Fisher-Yates algorithm
public export
shuffle : List a -> RandomResult (List a)
shuffle [] = Right []
shuffle xs = shuffleImpl (fromList xs) (length xs)
  where
    shuffleImpl : Vect n a -> (remaining : Nat) -> RandomResult (List a)
    shuffleImpl v Z = Right (toList v)
    shuffleImpl v (S r) = do
      idx <- randomNat (S r)
      -- Would need to implement swap - stub for now
      Right (toList v)

||| Pick n random elements from a list
public export
sample : (n : Nat) -> List a -> RandomResult (List a)
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

||| Generate a unique counter-based nonce (needs state)
||| For single-use nonces, prefer freshNonce
public export
counterNonce : (pfx : ByteVec 8) -> (counter : Bits64) -> ByteVec 12
counterNonce (MkByteVec pfxBytes) counter =
  let b3 : Bits8 = cast (shiftR counter 24)
      b2 : Bits8 = cast (shiftR counter 16)
      b1 : Bits8 = cast (shiftR counter 8)
      b0 : Bits8 = cast counter
  in MkByteVec (pfxBytes ++ [b3, b2, b1, b0])

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
