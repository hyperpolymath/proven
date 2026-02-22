# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeURL - FFI wrapper for proven_http_* URL encoding functions.
# All computation delegated to libproven; no logic reimplemented here.

package provide proven::url 1.0.0
package require Tcl 8.6
package require proven

namespace eval ::proven::url {
    namespace export encode decode
    namespace ensemble create

    # URL-encode a string (percent-encoding).
    # Returns dict {value <string> ok <bool> error <string>}.
    proc encode {input} {
        set data [encoding convertto utf-8 $input]
        set result [::proven::ffi::http_url_encode $data [string length $data]]
        return $result
    }

    # URL-decode a percent-encoded string.
    # Returns dict {value <string> ok <bool> error <string>}.
    proc decode {input} {
        set data [encoding convertto utf-8 $input]
        set result [::proven::ffi::http_url_decode $data [string length $data]]
        return $result
    }
}
