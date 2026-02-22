# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeUUID - FFI wrapper for proven_uuid_* functions.
# All computation delegated to libproven; no logic reimplemented here.

package provide proven::uuid 1.0.0
package require Tcl 8.6
package require proven

namespace eval ::proven::uuid {
    namespace export v4 to_string parse is_nil version
    namespace ensemble create

    # Generate a new UUID v4.
    # Returns dict {value <uuid_bytes> ok <bool> error <string>}.
    proc v4 {} {
        set result [::proven::ffi::uuid_v4]
        return $result
    }

    # Convert UUID bytes to canonical string representation.
    # Returns dict {value <string> ok <bool> error <string>}.
    proc to_string {uuidBytes} {
        set result [::proven::ffi::uuid_to_string $uuidBytes]
        return $result
    }

    # Parse UUID from canonical string format.
    # Returns dict {value <uuid_bytes> ok <bool> error <string>}.
    proc parse {uuidString} {
        set result [::proven::ffi::uuid_parse $uuidString [string length $uuidString]]
        return $result
    }

    # Check if UUID is the nil UUID (all zeros).
    proc is_nil {uuidBytes} {
        return [::proven::ffi::uuid_is_nil $uuidBytes]
    }

    # Get UUID version number.
    proc version {uuidBytes} {
        return [::proven::ffi::uuid_version $uuidBytes]
    }
}
