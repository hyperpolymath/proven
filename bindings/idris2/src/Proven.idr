-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Proven FFI: Self-hosted Idris 2 bindings for libproven.
|||
||| This is the main entry point for the proven-ffi package. It re-exports
||| all submodules so that users can import a single module to access the
||| full API:
|||
|||   ```idris
|||   import Proven
|||   ```
|||
||| Or import individual submodules for finer-grained control:
|||
|||   ```idris
|||   import Proven.SafeMath
|||   import Proven.SafeString
|||   ```
|||
||| Architecture:
|||   This package is a *self-hosted FFI binding*. It calls the precompiled
|||   libproven shared library via Idris 2's `%foreign "C"` mechanism rather
|||   than importing the Idris 2 source modules directly. This allows users
|||   to consume proven as a precompiled shared library without needing the
|||   full source tree.
|||
||| Initialisation:
|||   Before calling any proven function, the runtime must be initialised:
|||
|||   ```idris
|||   main : IO ()
|||   main = do
|||     status <- primIO prim__proven_init
|||     -- ... use proven functions ...
|||     primIO prim__proven_deinit
|||   ```
|||
||| Modules:
|||   - `Proven.FFI`          -- Raw %foreign declarations and status codes
|||   - `Proven.SafeMath`     -- Overflow-safe integer arithmetic
|||   - `Proven.SafeString`   -- UTF-8 validation, SQL/HTML/JS escaping
|||   - `Proven.SafePath`     -- Directory traversal detection, filename sanitisation
|||   - `Proven.SafeEmail`    -- RFC 5321 email validation
|||   - `Proven.SafeUrl`      -- URL parsing and validation
|||   - `Proven.SafeNetwork`  -- IPv4 address parsing and classification
|||   - `Proven.SafeCrypto`   -- Constant-time comparison, secure RNG, hex encoding
|||   - `Proven.SafeJson`     -- JSON validation and type detection
|||   - `Proven.SafeDateTime` -- ISO 8601 parsing, leap year, days in month
module Proven

import public Proven.FFI
import public Proven.SafeMath
import public Proven.SafeString
import public Proven.SafePath
import public Proven.SafeEmail
import public Proven.SafeUrl
import public Proven.SafeNetwork
import public Proven.SafeCrypto
import public Proven.SafeJson
import public Proven.SafeDateTime

%default total

-- ============================================================================
-- Lifecycle convenience wrappers
-- ============================================================================

||| Initialise the proven runtime.
|||
||| Must be called before any other proven function. Safe to call
||| multiple times (subsequent calls are no-ops).
||| Returns `Right ()` on success, `Left err` on failure.
public export
provenInit : HasIO io => io (Either ProvenError ())
provenInit = do
  status <- primIO prim__proven_init
  pure (statusToEither status)

||| Shut down the proven runtime.
|||
||| Call when done using proven functions. All allocated resources
||| should be freed before calling this.
public export
provenDeinit : HasIO io => io ()
provenDeinit = primIO prim__proven_deinit

||| Check whether the proven runtime is initialised.
public export
provenIsInitialized : HasIO io => io Bool
provenIsInitialized = do
  result <- primIO prim__proven_is_initialized
  pure (result /= 0)

-- ============================================================================
-- Version information
-- ============================================================================

||| Get the libproven ABI version number.
|||
||| This can be used for compatibility checking: if the ABI version
||| does not match what the binding was compiled against, the behaviour
||| is undefined.
public export
provenAbiVersion : HasIO io => io Int
provenAbiVersion = primIO prim__proven_ffi_abi_version

||| Get the libproven major version number.
public export
provenVersionMajor : HasIO io => io Int
provenVersionMajor = primIO prim__proven_version_major

||| Get the libproven minor version number.
public export
provenVersionMinor : HasIO io => io Int
provenVersionMinor = primIO prim__proven_version_minor

||| Get the libproven patch version number.
public export
provenVersionPatch : HasIO io => io Int
provenVersionPatch = primIO prim__proven_version_patch

||| Get the total number of modules in libproven.
public export
provenModuleCount : HasIO io => io Int
provenModuleCount = primIO prim__proven_module_count

-- ============================================================================
-- Bracket-style resource management
-- ============================================================================

||| Run an action with the proven runtime initialised.
|||
||| Initialises the runtime, runs the provided action, and ensures the
||| runtime is shut down afterwards (even if the action raises an
||| exception in IO).
|||
||| @ action The action to run with the proven runtime available
public export
withProven : HasIO io => (action : io a) -> io (Either ProvenError a)
withProven action = do
  initResult <- provenInit
  case initResult of
    Left err => pure (Left err)
    Right () => do
      result <- action
      provenDeinit
      pure (Right result)
