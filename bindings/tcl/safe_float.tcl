# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeFloat - FFI wrapper for proven_float_* functions.
# All computation delegated to libproven; no logic reimplemented here.

package provide proven::float 1.0.0
package require Tcl 8.6
package require proven

namespace eval ::proven::float {
    namespace export div is_finite is_nan sqrt ln
    namespace ensemble create

    # Safe division. Returns dict {value <double> ok <bool> error <string>}.
    proc div {a b} {
        set result [::proven::ffi::float_div [expr {double($a)}] [expr {double($b)}]]
        return $result
    }

    # Check if value is finite (not NaN or Inf).
    proc is_finite {x} {
        return [::proven::ffi::float_is_finite [expr {double($x)}]]
    }

    # Check if value is NaN.
    proc is_nan {x} {
        return [::proven::ffi::float_is_nan [expr {double($x)}]]
    }

    # Safe square root. Returns dict {value <double> ok <bool> error <string>}.
    proc sqrt {x} {
        set result [::proven::ffi::float_sqrt [expr {double($x)}]]
        return $result
    }

    # Safe natural logarithm. Returns dict {value <double> ok <bool> error <string>}.
    proc ln {x} {
        set result [::proven::ffi::float_ln [expr {double($x)}]]
        return $result
    }
}
