# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# Proven - Safe, validated operations library for Crystal.
#
# Provides type-safe operations across 38 modules organized by category:
#
# Core (11): SafeMath, SafeString, SafePath, SafeEmail, SafeUrl, SafeNetwork,
#            SafeCrypto, SafeUUID, SafeCurrency, SafePhone, SafeHex
#
# Data (7): SafeJson, SafeDateTime, SafeFloat, SafeVersion, SafeColor,
#           SafeAngle, SafeUnit
#
# Data Structures (5): SafeBuffer, SafeQueue, SafeBloom, SafeLRU, SafeGraph
#
# Resilience (4): SafeRateLimiter, SafeCircuitBreaker, SafeRetry, SafeMonotonic
#
# State (2): SafeStateMachine, SafeCalculator
#
# Algorithm (4): SafeGeo, SafeProbability, SafeChecksum, SafeTensor
#
# Security (2): SafePassword, SafeMl
#
# HTTP (3): SafeHeader, SafeCookie, SafeContentType

# Core modules
require "./proven/safe_math"
require "./proven/safe_string"
require "./proven/safe_path"
require "./proven/safe_email"
require "./proven/safe_url"
require "./proven/safe_network"
require "./proven/safe_crypto"
require "./proven/safe_uuid"
require "./proven/safe_currency"
require "./proven/safe_phone"
require "./proven/safe_hex"

# Data modules
require "./proven/safe_json"
require "./proven/safe_datetime"
require "./proven/safe_float"
require "./proven/safe_version"
require "./proven/safe_color"
require "./proven/safe_angle"
require "./proven/safe_unit"

# Data structures
require "./proven/safe_buffer"
require "./proven/safe_queue"
require "./proven/safe_bloom"
require "./proven/safe_lru"
require "./proven/safe_graph"

# Resilience
require "./proven/safe_rate_limiter"
require "./proven/safe_circuit_breaker"
require "./proven/safe_retry"
require "./proven/safe_monotonic"

# State
require "./proven/safe_state_machine"
require "./proven/safe_calculator"

# Algorithm
require "./proven/safe_geo"
require "./proven/safe_probability"
require "./proven/safe_checksum"
require "./proven/safe_tensor"

# Security
require "./proven/safe_password"
require "./proven/safe_ml"

# HTTP
require "./proven/safe_header"
require "./proven/safe_cookie"
require "./proven/safe_content_type"

module Proven
  VERSION      = "0.4.0"
  MODULE_COUNT = 38
end
