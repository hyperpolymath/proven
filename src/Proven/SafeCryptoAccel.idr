-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeCryptoAccel - Cryptographic accelerator safety
|||
||| This module provides type-safe primitives for hardware-accelerated
||| cryptographic operations. It covers algorithm selection, key specification
||| validation, expected key lengths, and post-quantum safety classification.
||| All functions are pure and total — misconfigured crypto parameters are
||| caught at validation time rather than causing hardware faults.
module Proven.SafeCryptoAccel

%default total

--------------------------------------------------------------------------------
-- Cryptographic Operations
--------------------------------------------------------------------------------

||| Cryptographic algorithms that may be hardware-accelerated
public export
data CryptoOp
  = AES128
  | AES256
  | ChaCha20
  | SHA256
  | SHA512
  | Ed25519
  | X25519
  | RSA2048
  | RSA4096

public export
Eq CryptoOp where
  AES128   == AES128   = True
  AES256   == AES256   = True
  ChaCha20 == ChaCha20 = True
  SHA256   == SHA256   = True
  SHA512   == SHA512   = True
  Ed25519  == Ed25519  = True
  X25519   == X25519   = True
  RSA2048  == RSA2048  = True
  RSA4096  == RSA4096  = True
  _        == _        = False

public export
Show CryptoOp where
  show AES128   = "AES-128"
  show AES256   = "AES-256"
  show ChaCha20 = "ChaCha20"
  show SHA256   = "SHA-256"
  show SHA512   = "SHA-512"
  show Ed25519  = "Ed25519"
  show X25519   = "X25519"
  show RSA2048  = "RSA-2048"
  show RSA4096  = "RSA-4096"

--------------------------------------------------------------------------------
-- Cryptographic Categories
--------------------------------------------------------------------------------

||| High-level classification of cryptographic primitives
public export
data CryptoCategory = Symmetric | Asymmetric | Hash | MAC | KDF

public export
Eq CryptoCategory where
  Symmetric  == Symmetric  = True
  Asymmetric == Asymmetric = True
  Hash       == Hash       = True
  MAC        == MAC        = True
  KDF        == KDF        = True
  _          == _          = False

public export
Show CryptoCategory where
  show Symmetric  = "Symmetric"
  show Asymmetric = "Asymmetric"
  show Hash       = "Hash"
  show MAC        = "MAC"
  show KDF        = "KDF"

||| Classify a cryptographic operation into its category
public export
categorise : CryptoOp -> CryptoCategory
categorise AES128   = Symmetric
categorise AES256   = Symmetric
categorise ChaCha20 = Symmetric
categorise SHA256   = Hash
categorise SHA512   = Hash
categorise Ed25519  = Asymmetric
categorise X25519   = Asymmetric
categorise RSA2048  = Asymmetric
categorise RSA4096  = Asymmetric

--------------------------------------------------------------------------------
-- Key Specification
--------------------------------------------------------------------------------

||| A specification for a cryptographic key
||| @ algorithm  The target algorithm
||| @ keyLength  Key length in bytes
||| @ ivLength   Initialisation vector length in bytes (Nothing for non-IV algorithms)
public export
record KeySpec where
  constructor MkKeySpec
  algorithm : CryptoOp
  keyLength : Nat
  ivLength  : Maybe Nat

public export
Show KeySpec where
  show ks = "KeySpec(" ++ show ks.algorithm
         ++ ", key=" ++ show ks.keyLength ++ "B"
         ++ ", iv=" ++ maybe "none" (\n => show n ++ "B") ks.ivLength ++ ")"

--------------------------------------------------------------------------------
-- Expected Key Lengths
--------------------------------------------------------------------------------

||| Return the expected key length in bytes for a given algorithm
public export
expectedKeyLength : CryptoOp -> Nat
expectedKeyLength AES128   = 16   -- 128 bits
expectedKeyLength AES256   = 32   -- 256 bits
expectedKeyLength ChaCha20 = 32   -- 256 bits
expectedKeyLength SHA256   = 0    -- Hash: no key
expectedKeyLength SHA512   = 0    -- Hash: no key
expectedKeyLength Ed25519  = 32   -- 256-bit private key
expectedKeyLength X25519   = 32   -- 256-bit private key
expectedKeyLength RSA2048  = 256  -- 2048-bit key = 256 bytes
expectedKeyLength RSA4096  = 512  -- 4096-bit key = 512 bytes

||| Return the expected IV/nonce length in bytes, or Nothing for algorithms
||| that do not use an IV
public export
expectedIVLength : CryptoOp -> Maybe Nat
expectedIVLength AES128   = Just 16   -- 128-bit IV (CBC) or 12 (GCM)
expectedIVLength AES256   = Just 16
expectedIVLength ChaCha20 = Just 12   -- 96-bit nonce
expectedIVLength _        = Nothing

--------------------------------------------------------------------------------
-- Quantum Safety
--------------------------------------------------------------------------------

||| Determine whether a given algorithm is considered quantum-safe
||| All current asymmetric and classic symmetric algorithms are NOT quantum-safe
||| by default; only AES-256 and SHA-512 are considered quantum-resistant at
||| 128-bit post-quantum security level via Grover's bound.
public export
isQuantumSafe : CryptoOp -> Bool
isQuantumSafe AES256 = True   -- 128-bit post-quantum via Grover
isQuantumSafe SHA512 = True   -- 256-bit post-quantum via Grover
isQuantumSafe _      = False

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Validate a key specification against the expected parameters for
||| the target algorithm. Checks key length and IV length.
public export
validateKeySpec : KeySpec -> Bool
validateKeySpec ks =
  let expectedKey = expectedKeyLength ks.algorithm
      expectedIV  = expectedIVLength ks.algorithm
  in ks.keyLength == expectedKey && ks.ivLength == expectedIV
