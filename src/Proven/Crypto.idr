-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Proven.Crypto - Convenience re-export module for cryptography and encoding safety
|||
||| Groups all cryptography-related Safe* modules into a single import for
||| applications that handle hashing, encryption, key management, checksums,
||| message digests, finite field arithmetic, base64/hex encoding, password
||| hashing and policy, X.509 certificate validation, and hardware attestation.
|||
||| Usage:
|||   import Proven.Crypto
|||
||| This single import provides access to all cryptographic safety types,
||| constructors, and validation functions without needing 10 separate imports.
module Proven.Crypto

import public Proven.SafeCrypto
import public Proven.SafeCryptoAccel
import public Proven.SafeChecksum
import public Proven.SafeDigest
import public Proven.SafeFiniteField
import public Proven.SafeBase64
import public Proven.SafeHex
import public Proven.SafePassword
import public Proven.SafeCert
import public Proven.SafeAttestation
