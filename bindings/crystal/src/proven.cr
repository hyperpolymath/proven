# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# Proven - Safe, validated operations library for Crystal.
#
# Provides type-safe operations with overflow checking, XSS prevention,
# path traversal protection, email validation, IP classification, and
# cryptographic operations.

require "./proven/safe_math"
require "./proven/safe_string"
require "./proven/safe_path"
require "./proven/safe_email"
require "./proven/safe_network"
require "./proven/safe_crypto"

module Proven
  VERSION = "0.1.0"
end
