# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven do
  @moduledoc """
  Safety-first utility functions with formal verification guarantees.

  Provides safe operations for:
  - Math: Division, modulo, checked arithmetic with overflow detection
  - Strings: HTML/SQL/JS/URL escaping, safe truncation
  - Paths: Traversal detection and filename sanitization
  - Email: Validation, parsing, normalization
  - URLs: Parsing with full component extraction
  - Network: IPv4 parsing, private/loopback/public classification
  - Crypto: Constant-time comparison, secure zeroing
  - UUID: Parsing, validation, formatting, constant-time comparison
  - Currency: Money arithmetic with overflow protection, formatting
  - Phone: E.164 parsing, national/international formatting
  - Hex: Encoding, decoding, constant-time comparison
  """
end
