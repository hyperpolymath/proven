# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven -- FFI bindings to libproven (formally verified safety primitives).
#
# All computation is performed in the Idris2/Zig core via the C ABI exposed by
# libproven.  This Nim package is a thin wrapper that marshals data across
# the FFI boundary and returns idiomatic Nim types (Option for errors).
#
# Link with: -lproven

import proven/lib_proven
export lib_proven

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
import proven/safe_json
import proven/safe_datetime
import proven/safe_float
import proven/safe_version
import proven/safe_color
import proven/safe_angle
import proven/safe_unit

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
export safe_json
export safe_datetime
export safe_float
export safe_version
export safe_color
export safe_angle
export safe_unit
