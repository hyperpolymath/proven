# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven do
  @moduledoc """
  Safety-first utility functions with formal verification guarantees.

  This is a thin FFI wrapper over `libproven`, which provides formally verified
  operations implemented in Idris 2 with dependent types and totality checking,
  exposed through a stable C ABI via the Zig FFI layer.

  ALL computation is performed in the Idris 2 core:
    Elixir -> Rustler NIF -> libproven.so -> Zig FFI -> Idris 2 RefC

  Provides safe operations for:
  - Math: Division, modulo, checked arithmetic with overflow detection
  - Strings: HTML/SQL/JS escaping via verified implementation
  - Paths: Traversal detection and filename sanitization
  - Email: Validation via RFC 5321 implementation
  - URLs: Parsing with full component extraction
  - Network: IPv4 parsing, private/loopback/public classification
  - Crypto: Constant-time comparison, secure random bytes
  - UUID: Parsing, validation, formatting
  - Currency: Money arithmetic with overflow protection
  - Phone: E.164 parsing and formatting
  - Hex: Encoding, decoding, constant-time comparison
  - Float: NaN/Infinity safe operations
  - Color: RGB/HSL conversion, WCAG contrast
  - Angle: Degree/radian conversion and normalization
  - Unit: Length, temperature, and other unit conversions
  - Geo: Coordinate validation, Haversine distance
  - Checksum: CRC-32 and other checksums
  - Probability: Clamped probability operations
  - ML: Activation functions (sigmoid, relu)
  - JSON: Validation via verified parser
  """
end
