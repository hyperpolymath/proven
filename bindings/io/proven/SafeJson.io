# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeJson.io - JSON validation for the Io language.
#
# All operations delegate to libproven via the native C addon (IoProven.c).
# Returns nil on error.
#
# Usage:
#   Proven init
#   valid := Proven jsonIsValid("{\"key\": 42}")
#   valid println   # true
#   type := Proven jsonGetType("[1, 2, 3]")
#   type println    # "array"
#   Proven deinit

SafeJson := Proven clone do(
    //doc SafeJson category Safety
    //doc SafeJson description JSON validation and type detection.

    //doc SafeJson jsonIsValid(json_str) Check if a string is valid JSON.
    # jsonIsValid is provided by the native C addon (IoProven.c).

    //doc SafeJson jsonGetType(json_str) Get the root-level JSON value type as a string.
    # jsonGetType is provided by the native C addon (IoProven.c).
)
