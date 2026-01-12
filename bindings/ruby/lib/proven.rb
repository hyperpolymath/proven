# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

# Proven - Safety-first utility functions with formal verification guarantees.
#
# Provides safe operations for:
# - Math: Division, modulo, checked arithmetic with overflow detection
# - Strings: HTML/SQL/JS/URL escaping, safe truncation
# - Paths: Traversal detection and filename sanitization
# - Email: Validation, parsing, normalization
# - Network: IPv4 parsing, private/loopback/public classification
# - Crypto: Constant-time comparison, secure zeroing
# - UUID: RFC 4122 UUID generation, parsing, and validation
# - Currency: ISO 4217 currency codes, type-safe money operations
# - Phone: E.164 phone number parsing and formatting
# - Hex: Hexadecimal encoding/decoding with constant-time comparison
module Proven
  VERSION = "0.4.0"
end

require_relative "proven/safe_math"
require_relative "proven/safe_string"
require_relative "proven/safe_path"
require_relative "proven/safe_email"
require_relative "proven/safe_network"
require_relative "proven/safe_crypto"
require_relative "proven/safe_uuid"
require_relative "proven/safe_currency"
require_relative "proven/safe_phone"
require_relative "proven/safe_hex"
