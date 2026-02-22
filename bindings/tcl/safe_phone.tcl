# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafePhone - FFI wrapper for proven_phone_* functions.
# All computation delegated to libproven; no logic reimplemented here.

package provide proven::phone 1.0.0
package require Tcl 8.6
package require proven

namespace eval ::proven::phone {
    namespace export parse format_e164
    namespace ensemble create

    # Parse phone number from string.
    # Returns dict {country_code <int> national_number <int> is_valid <bool>
    #               ok <bool> error <string>}.
    proc parse {input} {
        set data [encoding convertto utf-8 $input]
        set result [::proven::ffi::phone_parse $data [string length $data]]
        return $result
    }

    # Format phone number in E.164 format (+CCNNN...).
    # Takes country code and national number as integers.
    # Returns dict {value <string> ok <bool> error <string>}.
    proc format_e164 {countryCode nationalNumber} {
        set result [::proven::ffi::phone_format_e164 $countryCode $nationalNumber]
        return $result
    }
}
