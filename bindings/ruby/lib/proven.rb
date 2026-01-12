# SPDX-License-Identifier: PMPL-1.0
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
module Proven
  VERSION = "0.3.0"
end

require_relative "proven/safe_math"
require_relative "proven/safe_string"
require_relative "proven/safe_path"
require_relative "proven/safe_email"
require_relative "proven/safe_network"
require_relative "proven/safe_crypto"
