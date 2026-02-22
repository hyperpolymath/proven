# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeVersion - FFI wrapper for proven_version_* functions.
# All computation delegated to libproven; no logic reimplemented here.

package provide proven::version 1.0.0
package require Tcl 8.6
package require proven

namespace eval ::proven::version {
    namespace export parse compare
    namespace ensemble create

    # Parse semantic version string.
    # Returns dict {major <int> minor <int> patch <int> prerelease <string>
    #               ok <bool> error <string>}.
    proc parse {versionString} {
        set data [encoding convertto utf-8 $versionString]
        set result [::proven::ffi::version_parse $data [string length $data]]
        return $result
    }

    # Compare two semantic versions.
    # Takes two version dicts as returned by parse.
    # Returns -1 if a < b, 0 if equal, 1 if a > b.
    proc compare {versionA versionB} {
        return [::proven::ffi::version_compare $versionA $versionB]
    }
}
