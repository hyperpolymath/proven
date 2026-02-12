# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeFloat - Safe floating-point operations with NaN/Inf handling.
#

package provide proven::float 0.4.0
package require Tcl 8.6

namespace eval ::proven::float {
    namespace export is_finite is_nan is_inf safe_add safe_sub safe_mul safe_div
    namespace export clamp in_range approximately_equal round_to
    namespace export safe_sqrt safe_log safe_exp parse_float
    namespace ensemble create

    # Epsilon for approximate comparisons
    variable EPSILON 1e-10

    # Check if value is finite (not NaN or Inf)
    proc is_finite {value} {
        if {![string is double -strict $value]} {
            return 0
        }
        if {$value != $value} {
            return 0  ;# NaN
        }
        if {$value == "Inf" || $value == "-Inf"} {
            return 0
        }
        return [expr {$value < Inf && $value > -Inf}]
    }

    # Check if value is NaN
    proc is_nan {value} {
        if {![string is double -strict $value]} {
            return 0
        }
        return [expr {$value != $value}]
    }

    # Check if value is infinite
    proc is_inf {value} {
        if {![string is double -strict $value]} {
            return 0
        }
        return [expr {$value == Inf || $value == -Inf}]
    }

    # Safe add with overflow checking
    # Returns dict with {value ok error}
    proc safe_add {a b} {
        if {![is_finite $a] || ![is_finite $b]} {
            return [dict create value 0.0 ok 0 error "Non-finite operand"]
        }

        set result [expr {double($a) + double($b)}]

        if {![is_finite $result]} {
            return [dict create value 0.0 ok 0 error "Result overflow"]
        }

        return [dict create value $result ok 1 error ""]
    }

    # Safe subtract with overflow checking
    proc safe_sub {a b} {
        if {![is_finite $a] || ![is_finite $b]} {
            return [dict create value 0.0 ok 0 error "Non-finite operand"]
        }

        set result [expr {double($a) - double($b)}]

        if {![is_finite $result]} {
            return [dict create value 0.0 ok 0 error "Result overflow"]
        }

        return [dict create value $result ok 1 error ""]
    }

    # Safe multiply with overflow checking
    proc safe_mul {a b} {
        if {![is_finite $a] || ![is_finite $b]} {
            return [dict create value 0.0 ok 0 error "Non-finite operand"]
        }

        set result [expr {double($a) * double($b)}]

        if {![is_finite $result]} {
            return [dict create value 0.0 ok 0 error "Result overflow"]
        }

        return [dict create value $result ok 1 error ""]
    }

    # Safe divide with zero and overflow checking
    proc safe_div {a b} {
        if {![is_finite $a] || ![is_finite $b]} {
            return [dict create value 0.0 ok 0 error "Non-finite operand"]
        }

        if {$b == 0.0} {
            return [dict create value 0.0 ok 0 error "Division by zero"]
        }

        set result [expr {double($a) / double($b)}]

        if {![is_finite $result]} {
            return [dict create value 0.0 ok 0 error "Result overflow"]
        }

        return [dict create value $result ok 1 error ""]
    }

    # Clamp value to range
    proc clamp {value minVal maxVal} {
        if {$value < $minVal} {
            return $minVal
        }
        if {$value > $maxVal} {
            return $maxVal
        }
        return $value
    }

    # Check if value is in range (inclusive)
    proc in_range {value minVal maxVal} {
        return [expr {$value >= $minVal && $value <= $maxVal}]
    }

    # Check if two values are approximately equal
    proc approximately_equal {a b {epsilon ""}} {
        variable EPSILON

        if {$epsilon eq ""} {
            set epsilon $EPSILON
        }

        if {![is_finite $a] || ![is_finite $b]} {
            return 0
        }

        set diff [expr {abs($a - $b)}]

        # Absolute comparison for values near zero
        if {$diff <= $epsilon} {
            return 1
        }

        # Relative comparison for larger values
        set maxAbs [expr {max(abs($a), abs($b))}]
        return [expr {$diff <= $epsilon * $maxAbs}]
    }

    # Round to specified number of decimal places
    proc round_to {value decimalPlaces} {
        if {![is_finite $value]} {
            return $value
        }

        set multiplier [expr {pow(10, $decimalPlaces)}]
        return [expr {round($value * $multiplier) / $multiplier}]
    }

    # Safe square root
    proc safe_sqrt {value} {
        if {![is_finite $value]} {
            return [dict create value 0.0 ok 0 error "Non-finite operand"]
        }

        if {$value < 0} {
            return [dict create value 0.0 ok 0 error "Cannot sqrt negative number"]
        }

        return [dict create value [expr {sqrt($value)}] ok 1 error ""]
    }

    # Safe natural logarithm
    proc safe_log {value} {
        if {![is_finite $value]} {
            return [dict create value 0.0 ok 0 error "Non-finite operand"]
        }

        if {$value <= 0} {
            return [dict create value 0.0 ok 0 error "Cannot log non-positive number"]
        }

        return [dict create value [expr {log($value)}] ok 1 error ""]
    }

    # Safe exponential
    proc safe_exp {value} {
        if {![is_finite $value]} {
            return [dict create value 0.0 ok 0 error "Non-finite operand"]
        }

        # Prevent overflow for large values
        if {$value > 709} {
            return [dict create value 0.0 ok 0 error "Exponential overflow"]
        }

        set result [expr {exp($value)}]

        if {![is_finite $result]} {
            return [dict create value 0.0 ok 0 error "Result overflow"]
        }

        return [dict create value $result ok 1 error ""]
    }

    # Parse float from string
    proc parse_float {s} {
        set trimmed [string trim $s]

        if {$trimmed eq ""} {
            return [dict create value 0.0 ok 0 error "Empty string"]
        }

        if {![string is double -strict $trimmed]} {
            return [dict create value 0.0 ok 0 error "Invalid float format"]
        }

        set value [expr {double($trimmed)}]

        if {![is_finite $value]} {
            return [dict create value 0.0 ok 0 error "Non-finite result"]
        }

        return [dict create value $value ok 1 error ""]
    }
}
