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
||| @ SafeCommand  Injection-safe shell command construction
||| @ SafeSQL      SQL operations that prevent injection attacks
||| @ SafeJWT      JWT token validation without exceptions
||| @ SafeBase64   Base64 encoding/decoding with correctness proofs
||| @ SafeXML      XML parsing with XXE prevention
||| @ SafeYAML     YAML parsing with deserialization attack prevention
||| @ SafeTOML     TOML parsing with resource limits
||| @ SafeEnv      Environment variable access with sensitivity detection
||| @ SafeArgs     CLI argument parsing with validation
||| @ SafeFile     Bounded file I/O with path validation
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
import public Proven.SafeCommand
import public Proven.SafeSQL
import public Proven.SafeJWT
import public Proven.SafeBase64
import public Proven.SafeXML
import public Proven.SafeYAML
import public Proven.SafeTOML
import public Proven.SafeEnv
import public Proven.SafeArgs
import public Proven.SafeFile
