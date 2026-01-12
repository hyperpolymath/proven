-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Proven Core - Verified Safe Operations
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
||| @ SafeDateTime Date/time handling with timezone safety
||| @ SafeNetwork  IP addresses and network operations
||| @ SafeHtml     HTML construction and XSS prevention
||| @ SafeRegex    Safe regular expression matching
module Proven

import public Proven.Core
import public Proven.SafeMath
import public Proven.SafeString
import public Proven.SafeJson
import public Proven.SafeUrl
import public Proven.SafeEmail
import public Proven.SafePath
import public Proven.SafeCrypto
import public Proven.SafePassword
import public Proven.SafeDateTime
import public Proven.SafeNetwork
import public Proven.SafeHtml
import public Proven.SafeRegex
