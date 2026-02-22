# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Proven - Formally verified safety functions via FFI to libproven (Idris 2).
#
# This gem is a thin FFI wrapper. ALL computation happens in Idris 2 via the
# Zig FFI bridge (libproven.so). No safety logic is reimplemented in Ruby.
#
# Modules:
#   SafeMath       - Checked integer arithmetic (div, mod, add, sub, mul, etc.)
#   SafeString     - HTML/SQL/JS escaping, UTF-8 validation
#   SafePath       - Traversal detection, filename sanitization
#   SafeEmail      - Email validation (RFC 5321)
#   SafeNetwork    - IPv4 parsing, private/loopback/public classification
#   SafeCrypto     - Constant-time comparison, secure random bytes
#   SafeUuid       - UUID v4 generation, parsing, validation
#   SafeCurrency   - ISO 4217 currency parsing and formatting
#   SafePhone      - E.164 phone number parsing and formatting
#   SafeHex        - Hex encoding/decoding
#   SafeFloat      - Safe float division, sqrt, ln, NaN/Inf detection
#   SafeVersion    - Semantic versioning (parse, compare)
#   SafeColor      - Hex color parsing, RGB-to-HSL, hex formatting
#   SafeAngle      - Degree/radian conversion and normalization
#   SafeUnit       - Length and temperature unit conversion
#   SafeDatetime   - ISO 8601 parsing, formatting, leap year checks
#   SafeJson       - JSON validation and type detection
#   SafeUrl        - URL parsing into components
module Proven
  VERSION = "1.0.0"
end

# Load FFI layer first (required by all modules)
require_relative "proven/ffi"

# Load wrapper modules (each calls FFI, never reimplements logic)
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
require_relative "proven/safe_float"
require_relative "proven/safe_version"
require_relative "proven/safe_color"
require_relative "proven/safe_angle"
require_relative "proven/safe_unit"
require_relative "proven/safe_datetime"
require_relative "proven/safe_json"
require_relative "proven/safe_url"
