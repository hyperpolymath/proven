# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Proven: Formally verified safety primitives for Nim.
##
## This library provides safe operations with bounds checking,
## overflow detection, and security-focused string handling.

import proven/safe_math
import proven/safe_string
import proven/safe_path
import proven/safe_email
import proven/safe_network
import proven/safe_crypto

export safe_math
export safe_string
export safe_path
export safe_email
export safe_network
export safe_crypto
