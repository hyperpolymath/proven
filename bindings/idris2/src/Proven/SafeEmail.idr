-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Safe email validation via libproven FFI.
|||
||| Provides RFC 5321 email address validation. All logic is performed
||| by the formally verified Idris 2 core through the precompiled
||| shared library.
module Proven.SafeEmail

import Proven.FFI

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
-- Email validation
-- ============================================================================

||| Validate an email address per RFC 5321 (simplified).
|||
||| Returns `Just True` if the email is valid, `Just False` if it is
||| not, or `Nothing` on error (e.g. null pointer).
||| @ email The email address to validate
public export
isValidEmail : HasIO io => (email : String) -> io (Maybe Bool)
isValidEmail email = do
  let len = cast {to=Int} (length email)
  result <- primIO $ prim__proven_email_is_valid email len
  pure (boolResultToMaybe result)

||| Validate an email address, returning a typed error on failure.
|||
||| Returns `Right True` if valid, `Right False` if invalid but parseable,
||| or `Left err` with the specific error that occurred.
||| @ email The email address to validate
public export
isValidEmailE : HasIO io => (email : String) -> io (Either ProvenError Bool)
isValidEmailE email = do
  let len = cast {to=Int} (length email)
  (status, boolVal) <- primIO $ prim__proven_email_is_valid email len
  case statusToError status of
    Nothing  => pure (Right (boolVal /= 0))
    Just err => pure (Left err)
