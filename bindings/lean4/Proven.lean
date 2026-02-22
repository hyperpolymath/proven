/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Proven - Formally verified safety library for Lean 4

Proven is a safety library whose core logic is implemented in formally
verified Idris 2 code with dependent types and totality checking. This
Lean 4 package provides thin FFI bindings that call the verified core via
`libproven` (the Zig FFI bridge exposing a stable C ABI).

**No logic is reimplemented in Lean.** Every operation delegates to the
verified Idris 2 code.

## Modules

| Module               | Description                                      |
|----------------------|--------------------------------------------------|
| `Proven.FFI`         | Raw `@[extern]` C FFI declarations               |
| `Proven.SafeMath`    | Checked integer arithmetic (add, sub, mul, div)   |
| `Proven.SafeString`  | UTF-8 validation, SQL/HTML/JS escaping            |
| `Proven.SafePath`    | Directory traversal detection, filename sanitize  |
| `Proven.SafeEmail`   | RFC 5321 email validation                         |
| `Proven.SafeUrl`     | URL parsing into components                       |
| `Proven.SafeNetwork` | IPv4 parsing and classification                   |
| `Proven.SafeCrypto`  | Constant-time eq, random bytes, hex, CRC32        |
| `Proven.SafeJson`    | JSON validation and type detection                |
| `Proven.SafeDateTime`| ISO 8601 parsing, formatting, calendar utilities  |
| `Proven.SafeFloat`   | Safe float division, sqrt, ln                     |

## Usage

```lean
import Proven

open Proven.SafeMath in
def example : IO Unit := do
  -- Initialize the Proven runtime (required once)
  let _ <- Proven.FFI.provenInit

  -- Safe addition with overflow detection
  match (<- add 9223372036854775800 100) with
  | .ok sum   => IO.println s!"Sum = {sum}"
  | .error e  => IO.println s!"Overflow: {e}"

  -- Clean up
  Proven.FFI.provenDeinit
```

## Linking

This library requires `libproven` to be installed and linkable. The Lake
build configuration adds `-lproven` to the linker flags. Ensure that
`libproven.so` (or `.a` / `.dylib`) is in your library search path, or
set `LIBRARY_PATH` / `LD_LIBRARY_PATH` accordingly.
-/

import Proven.FFI
import Proven.SafeMath
import Proven.SafeString
import Proven.SafePath
import Proven.SafeEmail
import Proven.SafeUrl
import Proven.SafeNetwork
import Proven.SafeCrypto
import Proven.SafeJson
import Proven.SafeDateTime
import Proven.SafeFloat

namespace Proven

/-- Initialize the Proven runtime. Must be called before any other Proven
    function. Safe to call multiple times. -/
def init : IO Unit := do
  let status <- FFI.provenInit
  if status != 0 then
    throw (IO.Error.userError s!"Proven runtime initialization failed with status {status}")

/-- Shut down the Proven runtime. All allocated Proven resources should be
    freed before calling this. -/
def deinit : IO Unit :=
  FFI.provenDeinit

/-- Check whether the Proven runtime is currently initialized. -/
def isInitialized : IO Bool :=
  FFI.provenIsInitialized

/-- Get the library version as a triple `(major, minor, patch)`. -/
def version : IO (UInt32 × UInt32 × UInt32) := do
  let major <- FFI.provenVersionMajor
  let minor <- FFI.provenVersionMinor
  let patch <- FFI.provenVersionPatch
  return (major, minor, patch)

/-- Get the library version as a string (e.g., "0.9.0"). -/
def versionString : IO String := do
  let (major, minor, patch) <- version
  return s!"{major}.{minor}.{patch}"

/-- Get the FFI ABI version for compatibility checking. -/
def abiVersion : IO UInt32 :=
  FFI.provenFfiAbiVersion

/-- Get the total number of modules in the library. -/
def moduleCount : IO UInt32 :=
  FFI.provenModuleCount

end Proven
