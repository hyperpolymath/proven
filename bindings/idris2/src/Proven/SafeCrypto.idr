-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Safe cryptographic operations via libproven FFI.
|||
||| Provides constant-time comparison and secure random byte generation.
||| All logic is performed by the formally verified Idris 2 core
||| through the precompiled shared library.
|||
||| These operations are critical for security. Constant-time comparison
||| prevents timing side-channel attacks; the random byte generator uses
||| the operating system's cryptographically secure PRNG.
module Proven.SafeCrypto

import Proven.FFI
import System.FFI

%default total

-- ============================================================================
-- Helpers
-- ============================================================================

||| Interpret a C boolean result tuple as a Maybe Bool.
boolResultToMaybe : (Int, Int) -> Maybe Bool
boolResultToMaybe (s, v) =
  if isOK s then Just (v /= 0)
  else Nothing

-- ============================================================================
-- Constant-time comparison
-- ============================================================================

||| Constant-time byte comparison (timing-attack safe).
|||
||| Compares two byte sequences in constant time, preventing timing
||| side-channel attacks. Returns `Just True` if the sequences are
||| identical, `Just False` if they differ (including length mismatch),
||| or `Nothing` on error.
|||
||| This is essential for comparing authentication tokens, HMAC digests,
||| password hashes, and other security-sensitive data.
||| @ a   First string to compare
||| @ b   Second string to compare
public export
constantTimeEq : HasIO io => (a : String) -> (b : String) -> io (Maybe Bool)
constantTimeEq a b = do
  let aLen = cast {to=Int} (length a)
  let bLen = cast {to=Int} (length b)
  result <- primIO $ prim__proven_crypto_constant_time_eq a aLen b bLen
  pure (boolResultToMaybe result)

||| Constant-time comparison returning a typed error.
|||
||| Returns `Right True` if equal, `Right False` if different,
||| `Left err` on error.
public export
constantTimeEqE : HasIO io => String -> String -> io (Either ProvenError Bool)
constantTimeEqE a b = do
  let aLen = cast {to=Int} (length a)
  let bLen = cast {to=Int} (length b)
  (status, boolVal) <- primIO $ prim__proven_crypto_constant_time_eq a aLen b bLen
  case statusToError status of
    Nothing  => pure (Right (boolVal /= 0))
    Just err => pure (Left err)

-- ============================================================================
-- Secure random bytes
-- ============================================================================

||| Fill a buffer with cryptographically secure random bytes.
|||
||| Uses the operating system's CSPRNG (e.g. /dev/urandom, getrandom(2)).
||| Returns `Right ()` on success, `Left err` on failure (e.g. if the
||| CSPRNG is unavailable).
||| @ bufPtr Pointer to the buffer to fill
||| @ len    Number of random bytes to generate
public export
randomBytes : HasIO io => (bufPtr : AnyPtr) -> (len : Int) -> io (Either ProvenError ())
randomBytes bufPtr len = do
  status <- primIO $ prim__proven_crypto_random_bytes bufPtr len
  pure (statusToEither status)

-- ============================================================================
-- Hex encoding (delegated to SafeCrypto for cryptographic use cases)
-- ============================================================================

||| Encode a string as lowercase hexadecimal.
|||
||| Returns `Just hexString` on success, `Nothing` on error.
||| Memory management is handled internally.
||| @ str The input string to hex-encode
public export
hexEncode : HasIO io => (str : String) -> io (Maybe String)
hexEncode str = do
  let len = cast {to=Int} (length str)
  (status, ptr, _resultLen) <- primIO $ prim__proven_hex_encode str len 0
  if isOK status
    then do
      let result = prim__getString (prim__castPtr ptr)
      primIO $ prim__proven_free_string ptr
      pure (Just result)
    else pure Nothing

||| Encode a string as uppercase hexadecimal.
|||
||| Returns `Just hexString` on success, `Nothing` on error.
||| @ str The input string to hex-encode
public export
hexEncodeUpper : HasIO io => (str : String) -> io (Maybe String)
hexEncodeUpper str = do
  let len = cast {to=Int} (length str)
  (status, ptr, _resultLen) <- primIO $ prim__proven_hex_encode str len 1
  if isOK status
    then do
      let result = prim__getString (prim__castPtr ptr)
      primIO $ prim__proven_free_string ptr
      pure (Just result)
    else pure Nothing
