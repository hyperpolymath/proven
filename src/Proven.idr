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
||| @ SafeHeader   HTTP header handling with injection prevention
||| @ SafeCookie   HTTP cookie handling with security enforcement
||| @ SafeContentType Content-Type parsing with MIME sniffing prevention
||| @ SafeUUID     RFC 4122 UUID generation and validation
||| @ SafeCurrency ISO 4217 currency codes and safe money arithmetic
||| @ SafePhone    E.164 phone number parsing and validation
||| @ SafeHex      Hexadecimal encoding/decoding with bounds checking
||| @ SafeAngle    Angle handling with wrapping, clamping, and conversions
||| @ SafeProbability Probability values with distribution operations
||| @ SafeUnit     Physical units with dimensional analysis
||| @ SafeFiniteField Finite field arithmetic (GF(p) and GF(2^n))
||| @ SafeMonotonic Monotonic counters, Lamport clocks, HLC, version vectors
||| @ SafeRateLimiter Token bucket, leaky bucket, sliding window rate limiting
||| @ SafeCircuitBreaker Circuit breaker pattern for fault tolerance
||| @ SafeRetry    Retry policies with exponential backoff and jitter
||| @ SafeQueue    Bounded queues, priority queues, ring buffers
||| @ SafeBloom    Bloom filters with false positive rate guarantees
||| @ SafeLRU      LRU caches with TTL support and statistics
||| @ SafeColor    Color handling with gamut clamping and WCAG compliance
||| @ SafeGeo      Geographic coordinates with distance and bearing calculations
||| @ SafeVersion  Semantic versioning parsing and comparison
||| @ SafeChecksum CRC, Adler-32, Fletcher, and hash functions
||| @ SafeMarkdown Markdown parsing with XSS prevention
||| @ SafeGit Git operations with command injection prevention
||| @ SafeDocker Docker operations with security validation
||| @ SafeDNS DNS record handling with homograph detection
||| @ SafeSSH SSH key handling with weak algorithm detection
||| @ SafeLog Structured logging with PII redaction
||| @ SafeI18n Internationalization with injection prevention
||| @ SafeTemplate Template rendering with SSTI prevention
||| @ SafeArchive Archive handling with Zip Slip prevention
||| @ SafeCert X.509 certificate handling with validation
||| @ SafeOAuth OAuth 2.0/OIDC with CSRF prevention
||| @ SafeWebhook Webhook handling with signature verification
||| @ SafeCron Cron expressions with frequency limits
||| @ SafeBibTeX BibTeX parsing with LaTeX injection prevention
||| @ SafeMCP Model Context Protocol with prompt injection detection
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
import public Proven.SafeHeader
import public Proven.SafeCookie
import public Proven.SafeContentType
import public Proven.SafeUUID
import public Proven.SafeCurrency
import public Proven.SafePhone
import public Proven.SafeHex
import public Proven.SafeAngle
import public Proven.SafeProbability
import public Proven.SafeUnit
import public Proven.SafeFiniteField
import public Proven.SafeMonotonic
import public Proven.SafeRateLimiter
import public Proven.SafeCircuitBreaker
import public Proven.SafeRetry
import public Proven.SafeQueue
import public Proven.SafeBloom
import public Proven.SafeLRU
import public Proven.SafeColor
import public Proven.SafeGeo
import public Proven.SafeVersion
import public Proven.SafeChecksum
import public Proven.SafeMarkdown
import public Proven.SafeGit
import public Proven.SafeDocker
import public Proven.SafeDNS
import public Proven.SafeSSH
import public Proven.SafeLog
import public Proven.SafeI18n
import public Proven.SafeTemplate
import public Proven.SafeArchive
import public Proven.SafeCert
import public Proven.SafeOAuth
import public Proven.SafeWebhook
import public Proven.SafeCron
import public Proven.SafeBibTeX
import public Proven.SafeMCP
