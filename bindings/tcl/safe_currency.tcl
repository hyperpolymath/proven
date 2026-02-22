# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeCurrency - FFI wrapper for proven_currency_* functions.
# All computation delegated to libproven; no logic reimplemented here.

package provide proven::currency 1.0.0
package require Tcl 8.6
package require proven

namespace eval ::proven::currency {
    namespace export parse format
    namespace ensemble create

    # Parse currency amount from string.
    # Returns dict {amount_minor <int> currency_code <string>
    #               decimal_places <int> ok <bool> error <string>}.
    proc parse {input} {
        set data [encoding convertto utf-8 $input]
        set result [::proven::ffi::currency_parse $data [string length $data]]
        return $result
    }

    # Format currency amount as string.
    # Takes amount in minor units, 3-character currency code, and decimal places.
    # Returns dict {value <string> ok <bool> error <string>}.
    proc format {amountMinor currencyCode decimalPlaces} {
        set result [::proven::ffi::currency_format $amountMinor $currencyCode $decimalPlaces]
        return $result
    }
}
