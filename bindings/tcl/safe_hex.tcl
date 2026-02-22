# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeHex - FFI wrapper for proven_hex_* functions.
# All computation delegated to libproven; no logic reimplemented here.

package provide proven::hex 1.0.0
package require Tcl 8.6
package require proven

namespace eval ::proven::hex {
    namespace export encode decode
    namespace ensemble create

    # Encode bytes to hex string.
    # Takes a byte string (binary data) and optional uppercase flag.
    # Returns dict {value <string> ok <bool> error <string>}.
    proc encode {data {uppercase 0}} {
        set result [::proven::ffi::hex_encode $data [string length $data] $uppercase]
        return $result
    }

    # Decode hex string to bytes.
    # Returns dict {value <binary> ok <bool> error <string>}.
    proc decode {hexString} {
        set result [::proven::ffi::hex_decode $hexString [string length $hexString]]
        return $result
    }
}
