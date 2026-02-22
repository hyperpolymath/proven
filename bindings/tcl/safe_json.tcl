# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeJson - FFI wrapper for proven_json_* functions.
# All computation delegated to libproven; no logic reimplemented here.

package provide proven::json 1.0.0
package require Tcl 8.6
package require proven

namespace eval ::proven::json {
    namespace export is_valid get_type
    namespace ensemble create

    # Validate JSON syntax.
    # Returns 1 if valid, 0 if invalid.
    proc is_valid {jsonString} {
        set data [encoding convertto utf-8 $jsonString]
        return [::proven::ffi::json_is_valid $data [string length $data]]
    }

    # Get the top-level JSON value type.
    # Returns integer type code (see libproven documentation).
    proc get_type {jsonString} {
        set data [encoding convertto utf-8 $jsonString]
        return [::proven::ffi::json_get_type $data [string length $data]]
    }
}
