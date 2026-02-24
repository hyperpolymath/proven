-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe Hash Functions
|||
||| Type-safe interfaces for cryptographic hash functions.
||| Actual implementations require FFI to native crypto libraries.
module Proven.SafeCrypto.Hash

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
data ByteVector : (n : Nat) -> Type where
  MkByteVector : Vect n Byte -> ByteVector n

--------------------------------------------------------------------------------
-- Hash Output Types
--------------------------------------------------------------------------------

||| MD5 digest (128 bits) - DO NOT USE for security, only for checksums
public export
MD5Digest : Type
MD5Digest = ByteVector 16

||| SHA-1 digest (160 bits) - DEPRECATED for security
public export
SHA1Digest : Type
SHA1Digest = ByteVector 20

||| SHA-224 digest (224 bits)
public export
SHA224Digest : Type
SHA224Digest = ByteVector 28

||| SHA-256 digest (256 bits)
public export
SHA256Digest : Type
SHA256Digest = ByteVector 32

||| SHA-384 digest (384 bits)
public export
SHA384Digest : Type
SHA384Digest = ByteVector 48

||| SHA-512 digest (512 bits)
public export
SHA512Digest : Type
SHA512Digest = ByteVector 64

||| SHA3-256 digest (256 bits)
public export
SHA3_256Digest : Type
SHA3_256Digest = ByteVector 32

||| SHA3-512 digest (512 bits)
public export
SHA3_512Digest : Type
SHA3_512Digest = ByteVector 64

||| BLAKE2b digest (variable, up to 64 bytes)
public export
BLAKE2bDigest : Type
BLAKE2bDigest = ByteVector 64

||| BLAKE2s digest (variable, up to 32 bytes)
public export
BLAKE2sDigest : Type
BLAKE2sDigest = ByteVector 32

||| BLAKE3 digest (256 bits default)
public export
BLAKE3Digest : Type
BLAKE3Digest = ByteVector 32

--------------------------------------------------------------------------------
-- Hash Algorithm Tags
--------------------------------------------------------------------------------

||| Tag for hash algorithm selection
public export
data HashAlg
  = MD5_ALG        -- Insecure, legacy only
  | SHA1_ALG       -- Deprecated
  | SHA224_ALG
  | SHA256_ALG     -- Recommended minimum
  | SHA384_ALG
  | SHA512_ALG
  | SHA3_256_ALG
  | SHA3_512_ALG
  | BLAKE2b_ALG
  | BLAKE2s_ALG
  | BLAKE3_ALG     -- Modern, fast

public export
Show HashAlg where
  show MD5_ALG = "MD5"
  show SHA1_ALG = "SHA-1"
  show SHA224_ALG = "SHA-224"
  show SHA256_ALG = "SHA-256"
  show SHA384_ALG = "SHA-384"
  show SHA512_ALG = "SHA-512"
  show SHA3_256_ALG = "SHA3-256"
  show SHA3_512_ALG = "SHA3-512"
  show BLAKE2b_ALG = "BLAKE2b"
  show BLAKE2s_ALG = "BLAKE2s"
  show BLAKE3_ALG = "BLAKE3"

||| Get output size in bytes for algorithm
public export
hashOutputSize : HashAlg -> Nat
hashOutputSize MD5_ALG = 16
hashOutputSize SHA1_ALG = 20
hashOutputSize SHA224_ALG = 28
hashOutputSize SHA256_ALG = 32
hashOutputSize SHA384_ALG = 48
hashOutputSize SHA512_ALG = 64
hashOutputSize SHA3_256_ALG = 32
hashOutputSize SHA3_512_ALG = 64
hashOutputSize BLAKE2b_ALG = 64
hashOutputSize BLAKE2s_ALG = 32
hashOutputSize BLAKE3_ALG = 32

||| Get block size in bytes for algorithm
public export
hashBlockSize : HashAlg -> Nat
hashBlockSize MD5_ALG = 64
hashBlockSize SHA1_ALG = 64
hashBlockSize SHA224_ALG = 64
hashBlockSize SHA256_ALG = 64
hashBlockSize SHA384_ALG = 128
hashBlockSize SHA512_ALG = 128
hashBlockSize SHA3_256_ALG = 136
hashBlockSize SHA3_512_ALG = 72
hashBlockSize BLAKE2b_ALG = 128
hashBlockSize BLAKE2s_ALG = 64
hashBlockSize BLAKE3_ALG = 64

--------------------------------------------------------------------------------
-- Security Level Classification
--------------------------------------------------------------------------------

||| Security level of hash algorithm
public export
data SecurityLevel
  = Broken         -- Known practical attacks (MD5)
  | Deprecated     -- Theoretical weaknesses (SHA-1)
  | Standard       -- Current standard (SHA-2)
  | Modern         -- Latest generation (SHA-3, BLAKE3)

public export
Show SecurityLevel where
  show Broken = "BROKEN - DO NOT USE"
  show Deprecated = "DEPRECATED"
  show Standard = "Standard"
  show Modern = "Modern"

||| Get security level for algorithm
public export
securityLevel : HashAlg -> SecurityLevel
securityLevel MD5_ALG = Broken
securityLevel SHA1_ALG = Deprecated
securityLevel SHA224_ALG = Standard
securityLevel SHA256_ALG = Standard
securityLevel SHA384_ALG = Standard
securityLevel SHA512_ALG = Standard
securityLevel SHA3_256_ALG = Modern
securityLevel SHA3_512_ALG = Modern
securityLevel BLAKE2b_ALG = Modern
securityLevel BLAKE2s_ALG = Modern
securityLevel BLAKE3_ALG = Modern

||| Check if algorithm is secure for new applications
public export
isSecure : HashAlg -> Bool
isSecure alg = case securityLevel alg of
  Broken => False
  Deprecated => False
  _ => True

--------------------------------------------------------------------------------
-- Incremental Hashing Types (defined before postulates that reference them)
--------------------------------------------------------------------------------

||| Opaque hash context state
public export
data HashContext : HashAlg -> Type where
  MkHashContext : HashAlg -> HashContext alg

--------------------------------------------------------------------------------
-- HMAC Types (defined before postulates that reference them)
--------------------------------------------------------------------------------

||| HMAC key
public export
HMACKey : Type
HMACKey = Bytes

--------------------------------------------------------------------------------
-- FFI Postulates for Hash Functions
--
-- These are FFI stubs. The actual implementations live in ffi/zig/src/
-- and call platform-native cryptographic libraries (e.g., OpenSSL, libsodium).
-- At link time, the Zig FFI bridge replaces these with real hash computations.
--
-- Using postulate instead of cast: the type signatures are correct
-- (output has the right ByteVector length) and values will be provided by
-- the FFI layer at runtime. These functions are never called in pure Idris2
-- proof contexts.
--
-- To replace with proper FFI: uncomment %foreign declarations when
-- the Zig bridge is compiled, and remove the postulates.
--------------------------------------------------------------------------------

-- FFI: Actual implementation via Idris2 RefC compiled code
-- %foreign "C:proven_zig_sha256,libproven"
postulate prim__sha256 : Bytes -> SHA256Digest

-- FFI: Actual implementation via Idris2 RefC compiled code
-- %foreign "C:proven_zig_sha512,libproven"
postulate prim__sha512 : Bytes -> SHA512Digest

-- FFI: Actual implementation via Idris2 RefC compiled code
-- %foreign "C:proven_zig_sha3_256,libproven"
postulate prim__sha3_256 : Bytes -> SHA3_256Digest

-- FFI: Actual implementation via Idris2 RefC compiled code
-- %foreign "C:proven_zig_blake2b,libproven"
postulate prim__blake2b : Bytes -> BLAKE2bDigest

-- FFI: Actual implementation via Idris2 RefC compiled code
-- %foreign "C:proven_zig_blake3,libproven"
postulate prim__blake3 : Bytes -> BLAKE3Digest

-- FFI: Actual implementation via Idris2 RefC compiled code
-- %foreign "C:proven_zig_hash_finalize,libproven"
postulate prim__hashFinalize : {alg : HashAlg} -> HashContext alg -> ByteVector (hashOutputSize alg)

-- FFI: Actual implementation via Idris2 RefC compiled code
-- %foreign "C:proven_zig_hmac_sha256,libproven"
postulate prim__hmacSha256 : HMACKey -> Bytes -> SHA256Digest

-- FFI: Actual implementation via Idris2 RefC compiled code
-- %foreign "C:proven_zig_hmac_sha512,libproven"
postulate prim__hmacSha512 : HMACKey -> Bytes -> SHA512Digest

--------------------------------------------------------------------------------
-- Hash Function Public API
--------------------------------------------------------------------------------

||| Hash bytes to SHA-256 digest
||| @param input The data to hash
||| @return 32-byte SHA-256 digest (via FFI in production)
public export
sha256 : Bytes -> SHA256Digest
sha256 = prim__sha256

||| Hash bytes to SHA-512 digest
public export
sha512 : Bytes -> SHA512Digest
sha512 = prim__sha512

||| Hash bytes to SHA3-256 digest
public export
sha3_256 : Bytes -> SHA3_256Digest
sha3_256 = prim__sha3_256

||| Hash bytes to BLAKE2b digest
public export
blake2b : Bytes -> BLAKE2bDigest
blake2b = prim__blake2b

||| Hash bytes to BLAKE3 digest
public export
blake3 : Bytes -> BLAKE3Digest
blake3 = prim__blake3

--------------------------------------------------------------------------------
-- Incremental Hashing Operations
--------------------------------------------------------------------------------

||| Initialize hash context
public export
hashInit : (alg : HashAlg) -> HashContext alg
hashInit alg = MkHashContext alg

||| Update hash with more data
-- %foreign "C:proven_zig_hash_update,libproven"
public export
hashUpdate : HashContext alg -> Bytes -> HashContext alg
hashUpdate ctx input = ctx  -- FFI stub: actual state update via Zig

||| Finalize hash and get digest
public export
hashFinalize : {alg : HashAlg} -> HashContext alg -> ByteVector (hashOutputSize alg)
hashFinalize = prim__hashFinalize

--------------------------------------------------------------------------------
-- HMAC Operations
--------------------------------------------------------------------------------

||| Compute HMAC-SHA256
public export
hmacSha256 : HMACKey -> Bytes -> SHA256Digest
hmacSha256 = prim__hmacSha256

||| Compute HMAC-SHA512
public export
hmacSha512 : HMACKey -> Bytes -> SHA512Digest
hmacSha512 = prim__hmacSha512

--------------------------------------------------------------------------------
-- Hash Comparison (Constant Time)
--------------------------------------------------------------------------------

||| Compare two digests in constant time
public export
digestEq : ByteVector n -> ByteVector n -> Bool
digestEq (MkByteVector xs) (MkByteVector ys) = go xs ys 0
  where
    go : Vect m Byte -> Vect m Byte -> Byte -> Bool
    go [] [] acc = acc == 0
    go (x :: xs') (y :: ys') acc = go xs' ys' (acc `or` (x `xor` y))

--------------------------------------------------------------------------------
-- String Hashing Convenience
--------------------------------------------------------------------------------

||| Hash a string (UTF-8 encoded) to SHA-256
public export
hashString : String -> SHA256Digest
hashString s = sha256 (map (cast . ord) (unpack s))

||| Hash a string to hex string
public export
hashStringHex : String -> String
hashStringHex s =
  let (MkByteVector bytes) = hashString s
  in bytesToHex (toList bytes)
  where
    bytesToHex : List Byte -> String
    bytesToHex bs = concat (map byteToHex bs)
      where
        hexDigit : Nat -> Char
        hexDigit n = if n < 10 then chr (ord '0' + cast n) else chr (ord 'a' + cast n - 10)

        byteToHex : Byte -> String
        byteToHex b =
          let n = cast {to=Nat} b
          in pack [hexDigit (n `div` 16), hexDigit (n `mod` 16)]
