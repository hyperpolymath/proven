# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven - Tcl FFI bindings to libproven (Idris 2 + Zig C ABI).
#
# This is the master package file. It loads the shared library and
# provides all sub-modules as thin FFI wrappers.
# No logic is reimplemented in Tcl; every call delegates to verified Idris code.

package provide proven 1.0.0
package require Tcl 8.6

# Load the shared library. This makes all proven_* C functions available.
# The library must be compiled with Tcl stubs support, or loaded as a
# plain shared object. We use critcl-style load if available, otherwise
# fall back to the 'load' command.
namespace eval ::proven {
    variable libloaded 0
    variable libpath ""

    # Attempt to locate and load libproven.
    proc load_library {} {
        variable libloaded
        variable libpath

        if {$libloaded} { return 1 }

        # Search paths for the shared library.
        set search_paths [list \
            [file join [file dirname [info script]] .. .. ffi zig zig-out lib] \
            [file join [file dirname [info script]] .. .. lib] \
            /usr/local/lib \
            /usr/lib \
        ]

        foreach dir $search_paths {
            set candidate [file join $dir libproven.so]
            if {[file exists $candidate]} {
                set libpath $candidate
                break
            }
            set candidate [file join $dir libproven.dylib]
            if {[file exists $candidate]} {
                set libpath $candidate
                break
            }
        }

        if {$libpath eq ""} {
            # Try system default search
            set libpath "libproven.so"
        }

        if {[catch {load $libpath proven} err]} {
            # Library load failed; functions will error on call.
            return 0
        }

        set libloaded 1
        return 1
    }

    # Load on package require.
    load_library
}
