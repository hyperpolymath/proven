-- SPDX-License-Identifier: Palimpsest-MPL
||| Bulletproof Core - Verified Safe Operations
|||
||| This module re-exports all Safe modules for convenient access.
||| Every function in this library is total and cannot crash.
|||
||| @ SafeMath    Arithmetic that cannot overflow or divide by zero
||| @ SafeString  Text operations that handle encoding correctly
||| @ SafeJson    JSON parsing that cannot throw exceptions
||| @ SafeUrl     URL parsing that prevents injection attacks
||| @ SafeEmail   Email validation per RFC 5321
||| @ SafePath    Filesystem paths that prevent traversal attacks
||| @ SafeCrypto  Cryptographic operations with secure defaults
||| @ SafePassword Password hashing with Argon2id
module Bulletproof

import public Bulletproof.Core
import public Bulletproof.SafeMath
import public Bulletproof.SafeString
import public Bulletproof.SafeJson
import public Bulletproof.SafeUrl
import public Bulletproof.SafeEmail
import public Bulletproof.SafePath
import public Bulletproof.SafeCrypto
import public Bulletproof.SafePassword
