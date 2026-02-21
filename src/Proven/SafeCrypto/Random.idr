-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Secure Random Number Generation
|||
||| Type-safe interfaces for cryptographically secure random number generation.
||| Actual implementations require FFI to system entropy sources.
module Proven.SafeCrypto.Random

import Proven.Core
import Data.List
import Data.Bits
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Basic Types
--------------------------------------------------------------------------------

||| Byte type
public export
Byte : Type
Byte = Bits8

||| Byte sequence
public export
Bytes : Type
Bytes = List Byte

||| Fixed-size byte vector
public export
data ByteVec : (n : Nat) -> Type where
  MkByteVec : Vect n Byte -> ByteVec n

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
-- Random Generation Interface (Stubs)
--------------------------------------------------------------------------------

||| Generate random bytes
||| Actual implementation via FFI to system CSPRNG
public export
randomBytes : (n : Nat) -> RandomResult (ByteVec n)
randomBytes n =
  if n > 1000000  -- Reasonable limit
    then Left (RequestTooLarge n)
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
    else Right (believe_me (MkByteVec (replicate n 0)))  -- Stub

||| Generate random bytes as list
public export
randomByteList : (n : Nat) -> RandomResult Bytes
randomByteList n = map (\(MkByteVec v) => toList v) (randomBytes n)

||| Generate a random natural number in range [0, max)
public export
randomNat : (max : Nat) -> {auto ok : IsSucc max} -> RandomResult Nat
randomNat max =
  if max == 0
    then Right 0
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
    else Right (believe_me 0)  -- Stub - needs uniform distribution

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
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
    else Right (believe_me 0)  -- Stub

||| Generate a random boolean
public export
randomBool : RandomResult Bool
randomBool = map (\n => n == 0) (randomNat 2)

||| Generate a random double in [0, 1)
public export
randomDouble : RandomResult Double
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
randomDouble = Right (believe_me 0.0)  -- Stub

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
    alphabet = fromList $ unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

    toBase64 : Bytes -> String
    toBase64 bytes = pack (go bytes [])
      where
        getChar : Nat -> Char
        getChar n = index (restrict 63 (cast n)) alphabet

        go : Bytes -> List Char -> List Char
        go [] acc = reverse acc
        go [b1] acc =
          let c1 = getChar (cast (shiftR b1 2))
              c2 = getChar (cast ((shiftL b1 4) `and` 0x3F))
          in reverse (c2 :: c1 :: acc)
        go [b1, b2] acc =
          let c1 = getChar (cast (shiftR b1 2))
              c2 = getChar (cast (((shiftL b1 4) `or` (shiftR b2 4)) `and` 0x3F))
              c3 = getChar (cast ((shiftL b2 2) `and` 0x3F))
          in reverse (c3 :: c2 :: c1 :: acc)
        go (b1 :: b2 :: b3 :: rest) acc =
          let c1 = getChar (cast (shiftR b1 2))
              c2 = getChar (cast (((shiftL b1 4) `or` (shiftR b2 4)) `and` 0x3F))
              c3 = getChar (cast (((shiftL b2 2) `or` (shiftR b3 6)) `and` 0x3F))
              c4 = getChar (cast (b3 `and` 0x3F))
          in go rest (c4 :: c3 :: c2 :: c1 :: acc)

||| Generate a random hex string
public export
randomHex : (bytes : Nat) -> RandomResult String
randomHex n = map bytesToHex (randomByteList n)
  where
    bytesToHex : Bytes -> String
    bytesToHex bs = concat (map byteToHex bs)
      where
        hexDigit : Nat -> Char
        hexDigit d = if d < 10 then chr (ord '0' + cast d) else chr (ord 'a' + cast d - 10)

        byteToHex : Byte -> String
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
          b6' = (b6 `and` 0x0F) `or` 0x40  -- Version 4
          b8' = (b8 `and` 0x3F) `or` 0x80  -- Variant 10xx
          bytes' = [b0,b1,b2,b3,b4,b5,b6',b7,b8',b9,b10,b11,b12,b13,b14,b15]
      in Right (formatUUID bytes')
    _ => Left (SystemError "Failed to generate 16 bytes")
  where
    formatUUID : Bytes -> String
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

        hexByte : Byte -> String
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
counterNonce : (prefix : ByteVec 8) -> (counter : Bits64) -> ByteVec 12
counterNonce (MkByteVec prefix) counter =
  let counterBytes = [ cast (shiftR counter 24)
                     , cast (shiftR counter 16)
                     , cast (shiftR counter 8)
                     , cast counter
                     ]
  in MkByteVec (prefix ++ fromList counterBytes)

--------------------------------------------------------------------------------
-- Seed Generation
--------------------------------------------------------------------------------

||| Generate a seed for deterministic PRNG
public export
generateSeed : (n : Nat) -> RandomResult (ByteVec n)
generateSeed = randomBytes

||| Mix additional entropy into seed
public export
mixEntropy : ByteVec n -> Bytes -> ByteVec n
mixEntropy (MkByteVec seed) extra =
  MkByteVec (zipWith xor seed (take n (extra ++ replicate n 0)))
  where
    n : Nat
    n = length seed
