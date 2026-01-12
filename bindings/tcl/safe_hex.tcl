# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeHex - Safe hexadecimal encoding and decoding.
#

package provide proven::hex 0.1.0
package require Tcl 8.6

namespace eval ::proven::hex {
    namespace export encode encodeUpper decode isValid isValidBytes
    namespace export constantTimeEqual formatSpaced formatColons
    namespace export intToHex hexToInt hexCharToNibble nibbleToHexChar
    namespace ensemble create

    # Hex character lookup tables
    variable HEX_CHARS_LOWER "0123456789abcdef"
    variable HEX_CHARS_UPPER "0123456789ABCDEF"

    # Check if character is valid hex digit
    proc _isHexDigit {char} {
        regexp {^[0-9a-fA-F]$} $char
    }

    # Convert hex character to nibble value (0-15)
    # Returns -1 on invalid character
    proc hexCharToNibble {char} {
        switch -exact -- $char {
            0 { return 0 }
            1 { return 1 }
            2 { return 2 }
            3 { return 3 }
            4 { return 4 }
            5 { return 5 }
            6 { return 6 }
            7 { return 7 }
            8 { return 8 }
            9 { return 9 }
            a - A { return 10 }
            b - B { return 11 }
            c - C { return 12 }
            d - D { return 13 }
            e - E { return 14 }
            f - F { return 15 }
            default { return -1 }
        }
    }

    # Convert nibble value (0-15) to lowercase hex character
    proc nibbleToHexChar {nibbleValue} {
        variable HEX_CHARS_LOWER
        if {$nibbleValue < 0 || $nibbleValue > 15} {
            return ""
        }
        return [string index $HEX_CHARS_LOWER $nibbleValue]
    }

    # Convert nibble value (0-15) to uppercase hex character
    proc _nibbleToHexCharUpper {nibbleValue} {
        variable HEX_CHARS_UPPER
        if {$nibbleValue < 0 || $nibbleValue > 15} {
            return ""
        }
        return [string index $HEX_CHARS_UPPER $nibbleValue]
    }

    # Encode bytes list to lowercase hex string
    # Takes list of byte values (0-255)
    # Returns dict with {hex ok error}
    proc encode {bytesList} {
        set result ""

        foreach byteValue $bytesList {
            if {![string is integer -strict $byteValue] ||
                $byteValue < 0 || $byteValue > 255} {
                return [dict create hex "" ok 0 \
                    error "Invalid byte value: $byteValue"]
            }

            set highNibble [expr {($byteValue >> 4) & 0x0F}]
            set lowNibble [expr {$byteValue & 0x0F}]

            append result [nibbleToHexChar $highNibble]
            append result [nibbleToHexChar $lowNibble]
        }

        return [dict create hex $result ok 1 error ""]
    }

    # Encode bytes list to uppercase hex string
    proc encodeUpper {bytesList} {
        set result ""

        foreach byteValue $bytesList {
            if {![string is integer -strict $byteValue] ||
                $byteValue < 0 || $byteValue > 255} {
                return [dict create hex "" ok 0 \
                    error "Invalid byte value: $byteValue"]
            }

            set highNibble [expr {($byteValue >> 4) & 0x0F}]
            set lowNibble [expr {$byteValue & 0x0F}]

            append result [_nibbleToHexCharUpper $highNibble]
            append result [_nibbleToHexCharUpper $lowNibble]
        }

        return [dict create hex $result ok 1 error ""]
    }

    # Decode hex string to bytes list
    # Returns dict with {bytes ok error}
    proc decode {hexString} {
        set hexLength [string length $hexString]

        if {$hexLength % 2 != 0} {
            return [dict create bytes {} ok 0 \
                error "Hex string has odd length"]
        }

        set bytesList {}

        for {set i 0} {$i < $hexLength} {incr i 2} {
            set highChar [string index $hexString $i]
            set lowChar [string index $hexString [expr {$i + 1}]]

            set highNibble [hexCharToNibble $highChar]
            set lowNibble [hexCharToNibble $lowChar]

            if {$highNibble < 0 || $lowNibble < 0} {
                return [dict create bytes {} ok 0 \
                    error "Invalid hex character"]
            }

            set byteValue [expr {($highNibble << 4) | $lowNibble}]
            lappend bytesList $byteValue
        }

        return [dict create bytes $bytesList ok 1 error ""]
    }

    # Check if string contains only valid hex characters
    proc isValid {hexString} {
        if {$hexString eq ""} {
            return 1
        }
        return [regexp {^[0-9a-fA-F]+$} $hexString]
    }

    # Check if string is valid hex with even length (complete bytes)
    proc isValidBytes {hexString} {
        if {[string length $hexString] % 2 != 0} {
            return 0
        }
        return [isValid $hexString]
    }

    # Constant-time comparison of hex strings (timing-safe)
    # Returns 1 if equal, 0 if not
    proc constantTimeEqual {hexA hexB} {
        if {[string length $hexA] != [string length $hexB]} {
            return 0
        }

        # Normalize to lowercase for comparison
        set lowerA [string tolower $hexA]
        set lowerB [string tolower $hexB]

        set diff 0
        set length [string length $lowerA]

        for {set i 0} {$i < $length} {incr i} {
            scan [string index $lowerA $i] %c charA
            scan [string index $lowerB $i] %c charB
            set diff [expr {$diff | ($charA ^ $charB)}]
        }

        return [expr {$diff == 0}]
    }

    # Format hex with spaces between bytes
    # Returns dict with {formatted ok error}
    proc formatSpaced {hexString} {
        if {[string length $hexString] % 2 != 0} {
            return [dict create formatted "" ok 0 \
                error "Hex string has odd length"]
        }

        if {$hexString eq ""} {
            return [dict create formatted "" ok 1 error ""]
        }

        set parts {}
        set hexLength [string length $hexString]

        for {set i 0} {$i < $hexLength} {incr i 2} {
            lappend parts [string range $hexString $i [expr {$i + 1}]]
        }

        return [dict create formatted [join $parts " "] ok 1 error ""]
    }

    # Format hex with colons between bytes (MAC address style)
    # Returns dict with {formatted ok error}
    proc formatColons {hexString} {
        if {[string length $hexString] % 2 != 0} {
            return [dict create formatted "" ok 0 \
                error "Hex string has odd length"]
        }

        if {$hexString eq ""} {
            return [dict create formatted "" ok 1 error ""]
        }

        set parts {}
        set hexLength [string length $hexString]

        for {set i 0} {$i < $hexLength} {incr i 2} {
            lappend parts [string range $hexString $i [expr {$i + 1}]]
        }

        return [dict create formatted [join $parts ":"] ok 1 error ""]
    }

    # Convert integer to hex string with minimum width
    proc intToHex {integerValue {minWidth 0}} {
        if {$minWidth <= 0} {
            return [format "%x" $integerValue]
        }
        return [format "%0${minWidth}x" $integerValue]
    }

    # Parse hex string to integer
    # Returns dict with {value ok error}
    proc hexToInt {hexString} {
        if {![isValid $hexString]} {
            return [dict create value 0 ok 0 \
                error "Invalid hex string"]
        }

        if {$hexString eq ""} {
            return [dict create value 0 ok 0 \
                error "Empty hex string"]
        }

        if {[catch {scan $hexString %llx value}]} {
            return [dict create value 0 ok 0 \
                error "Hex value too large"]
        }

        return [dict create value $value ok 1 error ""]
    }
}
