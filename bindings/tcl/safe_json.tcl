# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeJson - Safe JSON validation and operations.
#

package provide proven::json 0.4.0
package require Tcl 8.6

namespace eval ::proven::json {
    namespace export is_valid validate get_string get_number get_bool
    namespace export get_array get_object is_null escape_string
    namespace export parse_path extract_path
    namespace ensemble create

    # Validate JSON syntax without full parsing
    # Returns 1 if valid, 0 if invalid
    proc is_valid {jsonString} {
        set depthBrace 0
        set depthBracket 0
        set inString 0
        set escape 0
        set len [string length $jsonString]

        for {set i 0} {$i < $len} {incr i} {
            set c [string index $jsonString $i]

            if {$escape} {
                set escape 0
                continue
            }

            switch -exact -- $c {
                "\\" {
                    if {$inString} {
                        set escape 1
                    }
                }
                "\"" {
                    set inString [expr {!$inString}]
                }
                "\{" {
                    if {!$inString} {
                        incr depthBrace
                    }
                }
                "\}" {
                    if {!$inString} {
                        incr depthBrace -1
                        if {$depthBrace < 0} {
                            return 0
                        }
                    }
                }
                "\[" {
                    if {!$inString} {
                        incr depthBracket
                    }
                }
                "\]" {
                    if {!$inString} {
                        incr depthBracket -1
                        if {$depthBracket < 0} {
                            return 0
                        }
                    }
                }
            }
        }

        return [expr {$depthBrace == 0 && $depthBracket == 0 && !$inString}]
    }

    # Validate JSON and return dict with {valid error}
    proc validate {jsonString} {
        set trimmed [string trim $jsonString]

        if {$trimmed eq ""} {
            return [dict create valid 0 error "Empty JSON"]
        }

        set firstChar [string index $trimmed 0]
        set lastChar [string index $trimmed end]

        # Must start with { or [
        if {$firstChar ne "\{" && $firstChar ne "\["} {
            # Could be primitive: string, number, boolean, null
            if {$firstChar eq "\""} {
                # String
                if {$lastChar ne "\""} {
                    return [dict create valid 0 error "Unclosed string"]
                }
            } elseif {[regexp {^-?[0-9]} $firstChar]} {
                # Number
                if {![regexp {^-?[0-9]+\.?[0-9]*([eE][+-]?[0-9]+)?$} $trimmed]} {
                    return [dict create valid 0 error "Invalid number"]
                }
            } elseif {$trimmed eq "true" || $trimmed eq "false" || $trimmed eq "null"} {
                return [dict create valid 1 error ""]
            } else {
                return [dict create valid 0 error "Invalid JSON value"]
            }
        }

        if {[is_valid $trimmed]} {
            return [dict create valid 1 error ""]
        }

        return [dict create valid 0 error "Invalid JSON structure"]
    }

    # Check if value looks like a JSON string
    proc get_string {jsonValue} {
        set trimmed [string trim $jsonValue]
        if {[string index $trimmed 0] eq "\"" && [string index $trimmed end] eq "\""} {
            return [dict create value [string range $trimmed 1 end-1] ok 1]
        }
        return [dict create value "" ok 0]
    }

    # Check if value looks like a JSON number
    proc get_number {jsonValue} {
        set trimmed [string trim $jsonValue]
        if {[regexp {^-?[0-9]+\.?[0-9]*([eE][+-]?[0-9]+)?$} $trimmed]} {
            if {[string first "." $trimmed] >= 0 || [string first "e" [string tolower $trimmed]] >= 0} {
                return [dict create value [expr {double($trimmed)}] ok 1]
            }
            return [dict create value [expr {wide($trimmed)}] ok 1]
        }
        return [dict create value 0 ok 0]
    }

    # Check if value is JSON boolean
    proc get_bool {jsonValue} {
        set trimmed [string trim $jsonValue]
        if {$trimmed eq "true"} {
            return [dict create value 1 ok 1]
        }
        if {$trimmed eq "false"} {
            return [dict create value 0 ok 1]
        }
        return [dict create value 0 ok 0]
    }

    # Check if value is null
    proc is_null {jsonValue} {
        set trimmed [string trim $jsonValue]
        return [expr {$trimmed eq "null"}]
    }

    # Check if value looks like JSON array
    proc get_array {jsonValue} {
        set trimmed [string trim $jsonValue]
        if {[string index $trimmed 0] eq "\[" && [string index $trimmed end] eq "\]"} {
            return [dict create value $trimmed ok 1]
        }
        return [dict create value "" ok 0]
    }

    # Check if value looks like JSON object
    proc get_object {jsonValue} {
        set trimmed [string trim $jsonValue]
        if {[string index $trimmed 0] eq "\{" && [string index $trimmed end] eq "\}"} {
            return [dict create value $trimmed ok 1]
        }
        return [dict create value "" ok 0]
    }

    # Escape string for JSON
    proc escape_string {input} {
        set result ""
        set len [string length $input]

        for {set i 0} {$i < $len} {incr i} {
            set c [string index $input $i]
            switch -exact -- $c {
                "\\" { append result "\\\\" }
                "\"" { append result "\\\"" }
                "\n" { append result "\\n" }
                "\r" { append result "\\r" }
                "\t" { append result "\\t" }
                "\b" { append result "\\b" }
                "\f" { append result "\\f" }
                default {
                    scan $c %c code
                    if {$code < 32} {
                        append result [format "\\u%04x" $code]
                    } else {
                        append result $c
                    }
                }
            }
        }

        return $result
    }

    # Parse JSON path (e.g., "user.name" or "items[0].id")
    # Returns list of path components
    proc parse_path {path} {
        set components {}
        set current ""
        set len [string length $path]
        set i 0

        while {$i < $len} {
            set c [string index $path $i]

            if {$c eq "."} {
                if {$current ne ""} {
                    lappend components [dict create type key value $current]
                    set current ""
                }
            } elseif {$c eq "\["} {
                if {$current ne ""} {
                    lappend components [dict create type key value $current]
                    set current ""
                }
                # Find closing bracket
                set closePos [string first "\]" $path $i]
                if {$closePos < 0} {
                    return [dict create components {} ok 0 error "Unclosed bracket"]
                }
                set indexStr [string range $path [expr {$i + 1}] [expr {$closePos - 1}]]
                if {[string is integer -strict $indexStr]} {
                    lappend components [dict create type index value $indexStr]
                } else {
                    lappend components [dict create type key value $indexStr]
                }
                set i $closePos
            } else {
                append current $c
            }

            incr i
        }

        if {$current ne ""} {
            lappend components [dict create type key value $current]
        }

        return [dict create components $components ok 1 error ""]
    }

    # Simple JSON path extraction (for well-formed JSON only)
    # Note: This is a simplified implementation - production use should use a proper JSON parser
    proc extract_path {jsonString path} {
        set pathResult [parse_path $path]
        if {![dict get $pathResult ok]} {
            return [dict create value "" ok 0 error [dict get $pathResult error]]
        }

        # This is a placeholder - full implementation would require JSON parsing
        return [dict create value "" ok 0 error "Full path extraction requires JSON parser"]
    }
}
