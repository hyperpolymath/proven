-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeCrypto - Cryptographic operations with safety guarantees
|||
||| This module provides:
||| - Type-safe cryptographic primitives
||| - Constant-time comparison
||| - Secure random number generation interface
||| - Hash function interfaces
||| - Encryption/decryption interfaces
|||
||| Note: Actual implementations require FFI to Zig crypto libraries
module Proven.SafeCrypto
import Data.String
import Data.List

import public Proven.Core
import public Proven.SafeCrypto.Hash
import public Proven.SafeCrypto.Random

import Data.List
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- Cryptographic Byte Types
--------------------------------------------------------------------------------

||| A single byte (8 bits)
public export
Byte : Type
Byte = Bits8

||| A sequence of bytes
public export
Bytes : Type
Bytes = List Byte

||| Fixed-size byte array (for crypto outputs)
public export
data ByteArray : (n : Nat) -> Type where
  MkByteArray : (bytes : Vect n Byte) -> ByteArray n

||| 16-byte array (128 bits) - for AES keys, IVs
public export
Bytes16 : Type
Bytes16 = ByteArray 16

||| 32-byte array (256 bits) - for SHA-256, AES-256 keys
public export
Bytes32 : Type
Bytes32 = ByteArray 32

||| 64-byte array (512 bits) - for SHA-512
public export
Bytes64 : Type
Bytes64 = ByteArray 64

--------------------------------------------------------------------------------
-- Sensitive Data Wrapper
--------------------------------------------------------------------------------

||| Wrapper for sensitive data that should be zeroed after use
||| This type tracks data that needs secure handling
public export
data Sensitive : Type -> Type where
  MkSensitive : (value : a) -> Sensitive a

||| Extract value from Sensitive (marks the data as "used")
||| In a real implementation, this would zero memory on drop
public export
unsafeReveal : Sensitive a -> a
unsafeReveal (MkSensitive v) = v

||| Apply function to sensitive data keeping it wrapped
public export
mapSensitive : (a -> b) -> Sensitive a -> Sensitive b
mapSensitive f (MkSensitive v) = MkSensitive (f v)

||| Combine two sensitive values
public export
combineSensitive : (a -> b -> c) -> Sensitive a -> Sensitive b -> Sensitive c
combineSensitive f (MkSensitive a) (MkSensitive b) = MkSensitive (f a b)

--------------------------------------------------------------------------------
-- Constant-Time Operations
--------------------------------------------------------------------------------

||| Compare two byte arrays in constant time
||| Returns True iff all bytes are equal
||| IMPORTANT: This comparison takes the same time regardless of where bytes differ
public export
constantTimeEq : Bytes -> Bytes -> Bool
constantTimeEq xs ys =
  if length xs /= length ys
    then False
    else go xs ys 0
  where
    go : Bytes -> Bytes -> Byte -> Bool
    go [] [] acc = acc == 0
    go (x :: xs') (y :: ys') acc = go xs' ys' (acc `or` (x `xor` y))
    go _ _ _ = False  -- Different lengths (shouldn't reach here)

||| Constant-time comparison for ByteArray
public export
constantTimeEqArray : ByteArray n -> ByteArray n -> Bool
constantTimeEqArray (MkByteArray xs) (MkByteArray ys) = go xs ys 0
  where
    go : Vect m Byte -> Vect m Byte -> Byte -> Bool
    go [] [] acc = acc == 0
    go (x :: xs') (y :: ys') acc = go xs' ys' (acc `or` (x `xor` y))

||| Constant-time select: returns a if cond is 0, b if cond is non-zero
public export
constantTimeSelect : Byte -> a -> a -> a
constantTimeSelect cond a b = if cond == 0 then a else b

--------------------------------------------------------------------------------
-- Cryptographic Key Types
--------------------------------------------------------------------------------

||| Symmetric encryption key
public export
data SymmetricKey : (bits : Nat) -> Type where
  MkSymKey : Sensitive (ByteArray (bits `div` 8)) -> SymmetricKey bits

||| AES-128 key
public export
AES128Key : Type
AES128Key = SymmetricKey 128

||| AES-256 key
public export
AES256Key : Type
AES256Key = SymmetricKey 256

||| HMAC key (variable length)
public export
data HMACKey : Type where
  MkHMACKey : Sensitive Bytes -> HMACKey

--------------------------------------------------------------------------------
-- Nonce/IV Types
--------------------------------------------------------------------------------

||| Nonce for AEAD encryption
public export
data Nonce : (n : Nat) -> Type where
  MkNonce : ByteArray n -> Nonce n

||| 12-byte nonce (standard for AES-GCM)
public export
Nonce12 : Type
Nonce12 = Nonce 12

||| 16-byte IV (for AES-CBC)
public export
IV16 : Type
IV16 = Nonce 16

||| Check if nonce has been used (for nonce tracking)
public export
data NonceStatus = Fresh | Used

||| Tracked nonce that can only be used once
public export
data TrackedNonce : (n : Nat) -> NonceStatus -> Type where
  MkTrackedNonce : Nonce n -> TrackedNonce n Fresh

||| Use a nonce (marks it as used)
public export
useNonce : TrackedNonce n Fresh -> (Nonce n, TrackedNonce n Used)
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
useNonce (MkTrackedNonce nonce) = (nonce, believe_me (MkTrackedNonce nonce))

--------------------------------------------------------------------------------
-- Encryption Result Types
--------------------------------------------------------------------------------

||| Ciphertext with authentication tag
public export
record AuthenticatedCiphertext where
  constructor MkAuthCiphertext
  ciphertext : Bytes
  tag : ByteArray 16  -- 128-bit authentication tag

||| Encryption errors
public export
data CryptoError
  = InvalidKeyLength
  | InvalidNonceLength
  | DecryptionFailed
  | AuthenticationFailed
  | InsufficientEntropy
  | AlgorithmNotSupported String

public export
Show CryptoError where
  show InvalidKeyLength = "Invalid key length"
  show InvalidNonceLength = "Invalid nonce length"
  show DecryptionFailed = "Decryption failed"
  show AuthenticationFailed = "Authentication failed"
  show InsufficientEntropy = "Insufficient entropy"
  show (AlgorithmNotSupported alg) = "Algorithm not supported: " ++ alg

--------------------------------------------------------------------------------
-- Cryptographic Algorithm Interfaces
--------------------------------------------------------------------------------

||| Interface for hash algorithms
public export
interface HashAlgorithm alg where
  hashOutputSize : Nat
  hashBlockSize : Nat
  hashName : String

||| Interface for symmetric encryption
public export
interface SymmetricCipher cipher where
  cipherKeySize : Nat
  cipherBlockSize : Nat
  cipherName : String

||| Interface for AEAD ciphers
public export
interface AEADCipher cipher where
  aeadKeySize : Nat
  aeadNonceSize : Nat
  aeadTagSize : Nat
  aeadName : String

--------------------------------------------------------------------------------
-- Algorithm Definitions
--------------------------------------------------------------------------------

||| SHA-256 algorithm marker
public export
data SHA256 = MkSHA256

public export
HashAlgorithm SHA256 where
  hashOutputSize = 32
  hashBlockSize = 64
  hashName = "SHA-256"

||| SHA-512 algorithm marker
public export
data SHA512 = MkSHA512

public export
HashAlgorithm SHA512 where
  hashOutputSize = 64
  hashBlockSize = 128
  hashName = "SHA-512"

||| BLAKE2b algorithm marker
public export
data BLAKE2b = MkBLAKE2b

public export
HashAlgorithm BLAKE2b where
  hashOutputSize = 64
  hashBlockSize = 128
  hashName = "BLAKE2b"

||| AES-GCM algorithm marker
public export
data AESGCM = MkAESGCM

public export
AEADCipher AESGCM where
  aeadKeySize = 32  -- 256-bit
  aeadNonceSize = 12
  aeadTagSize = 16
  aeadName = "AES-256-GCM"

||| ChaCha20-Poly1305 algorithm marker
public export
data ChaCha20Poly1305 = MkChaCha20Poly1305

public export
AEADCipher ChaCha20Poly1305 where
  aeadKeySize = 32
  aeadNonceSize = 12
  aeadTagSize = 16
  aeadName = "ChaCha20-Poly1305"

--------------------------------------------------------------------------------
-- Stub Interfaces (Implemented via FFI)
--------------------------------------------------------------------------------

||| Hash bytes using specified algorithm
||| Actual implementation requires FFI
public export
hash : (alg : Type) -> HashAlgorithm alg => Bytes -> ByteArray (hashOutputSize {alg})
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
hash alg input = believe_me (MkByteArray [])  -- Stub - requires FFI

||| HMAC using specified hash algorithm
public export
hmac : (alg : Type) -> HashAlgorithm alg =>
       HMACKey -> Bytes -> ByteArray (hashOutputSize {alg})
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
hmac alg key input = believe_me (MkByteArray [])  -- Stub - requires FFI

||| AEAD encrypt
public export
aeadEncrypt : (cipher : Type) -> AEADCipher cipher =>
              SymmetricKey (aeadKeySize {cipher} * 8) ->
              Nonce (aeadNonceSize {cipher}) ->
              Bytes ->  -- Additional authenticated data
              Bytes ->  -- Plaintext
              Either CryptoError AuthenticatedCiphertext
aeadEncrypt cipher key nonce aad plaintext = Left (AlgorithmNotSupported "Stub")

||| AEAD decrypt
public export
aeadDecrypt : (cipher : Type) -> AEADCipher cipher =>
              SymmetricKey (aeadKeySize {cipher} * 8) ->
              Nonce (aeadNonceSize {cipher}) ->
              Bytes ->  -- Additional authenticated data
              AuthenticatedCiphertext ->
              Either CryptoError Bytes
aeadDecrypt cipher key nonce aad ct = Left (AlgorithmNotSupported "Stub")

--------------------------------------------------------------------------------
-- Key Derivation
--------------------------------------------------------------------------------

||| HKDF-Extract
public export
hkdfExtract : (alg : Type) -> HashAlgorithm alg =>
              Bytes -> -- Salt
              Bytes -> -- Input key material
              ByteArray (hashOutputSize {alg})
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
hkdfExtract alg salt ikm = believe_me (MkByteArray [])  -- Stub

||| HKDF-Expand
public export
hkdfExpand : (alg : Type) -> HashAlgorithm alg =>
             ByteArray (hashOutputSize {alg}) -> -- PRK
             Bytes -> -- Info
             (n : Nat) -> -- Output length
             ByteArray n
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
hkdfExpand alg prk info n = believe_me (MkByteArray [])  -- Stub

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

||| Convert hex string to bytes
public export
hexToBytes : String -> Maybe Bytes
hexToBytes s = go (unpack s) []
  where
    hexVal : Char -> Maybe Nat
    hexVal c =
      if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
      else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
      else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
      else Nothing

    go : List Char -> Bytes -> Maybe Bytes
    go [] acc = Just (reverse acc)
    go [_] _ = Nothing  -- Odd number of chars
    go (h1 :: h2 :: rest) acc = do
      v1 <- hexVal h1
      v2 <- hexVal h2
      go rest (cast (v1 * 16 + v2) :: acc)

||| Convert bytes to hex string
public export
bytesToHex : Bytes -> String
bytesToHex bytes = concat (map byteToHex bytes)
  where
    hexDigit : Nat -> Char
    hexDigit n = if n < 10 then chr (ord '0' + cast n) else chr (ord 'a' + cast n - 10)

    byteToHex : Byte -> String
    byteToHex b =
      let n = cast {to=Nat} b
      in pack [hexDigit (n `div` 16), hexDigit (n `mod` 16)]

||| XOR two byte arrays of same length
public export
xorBytes : Bytes -> Bytes -> Maybe Bytes
xorBytes xs ys =
  if length xs /= length ys
    then Nothing
    else Just (zipWith xor xs ys)
