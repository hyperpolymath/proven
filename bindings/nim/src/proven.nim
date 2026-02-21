# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Proven: Formally verified safety primitives for Nim.
##
## This library provides safe operations with bounds checking,
## overflow detection, and security-focused string handling.
##
## ModuleCount: 38

# Core modules (11)
import proven/safe_math
import proven/safe_string
import proven/safe_path
import proven/safe_email
import proven/safe_url
import proven/safe_network
import proven/safe_crypto
import proven/safe_uuid
import proven/safe_currency
import proven/safe_phone
import proven/safe_hex

# Data modules (7)
import proven/safe_json
import proven/safe_datetime
import proven/safe_float
import proven/safe_version
import proven/safe_color
import proven/safe_angle
import proven/safe_unit

# Data Structures modules (5)
import proven/safe_buffer
import proven/safe_queue
import proven/safe_bloom
import proven/safe_lru
import proven/safe_graph

# Resilience modules (4)
import proven/safe_rate_limiter
import proven/safe_circuit_breaker
import proven/safe_retry
import proven/safe_monotonic

# State modules (2)
import proven/safe_state_machine
import proven/safe_calculator

# Algorithm modules (4)
import proven/safe_geo
import proven/safe_probability
import proven/safe_checksum
import proven/safe_tensor

# Security modules (2)
import proven/safe_password
import proven/safe_ml

# HTTP modules (3)
import proven/safe_header
import proven/safe_cookie
import proven/safe_content_type

# Core exports (11)
export safe_math
export safe_string
export safe_path
export safe_email
export safe_url
export safe_network
export safe_crypto
export safe_uuid
export safe_currency
export safe_phone
export safe_hex

# Data exports (7)
export safe_json
export safe_datetime
export safe_float
export safe_version
export safe_color
export safe_angle
export safe_unit

# Data Structures exports (5)
export safe_buffer
export safe_queue
export safe_bloom
export safe_lru
export safe_graph

# Resilience exports (4)
export safe_rate_limiter
export safe_circuit_breaker
export safe_retry
export safe_monotonic

# State exports (2)
export safe_state_machine
export safe_calculator

# Algorithm exports (4)
export safe_geo
export safe_probability
export safe_checksum
export safe_tensor

# Security exports (2)
export safe_password
export safe_ml

# HTTP exports (3)
export safe_header
export safe_cookie
export safe_content_type
