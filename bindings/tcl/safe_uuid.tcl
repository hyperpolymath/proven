# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeUUID - UUID generation and validation following RFC 4122.
#

package provide proven::uuid 0.1.0
package require Tcl 8.6

namespace eval ::proven::uuid {
    namespace export parse format isValid version variant isNil fromBytes
    namespace export toUrn v4FromBytes
    namespace ensemble create

    # UUID version constants
    variable VERSION_NIL 0
    variable VERSION_1   1
    variable VERSION_2   2
    variable VERSION_3   3
    variable VERSION_4   4
    variable VERSION_5   5

    # UUID variant constants
    variable VARIANT_NCS      0
    variable VARIANT_RFC4122  1
    variable VARIANT_MICROSOFT 2
    variable VARIANT_FUTURE   3

    # Namespace UUIDs (RFC 4122)
    variable NAMESPACE_DNS "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
    variable NAMESPACE_URL "6ba7b811-9dad-11d1-80b4-00c04fd430c8"
    variable NAMESPACE_OID "6ba7b812-9dad-11d1-80b4-00c04fd430c8"
    variable NAMESPACE_X500 "6ba7b814-9dad-11d1-80b4-00c04fd430c8"

    # Nil UUID
    variable NIL_UUID "00000000-0000-0000-0000-000000000000"

    # Check if character is valid hex digit
    proc _isHexChar {char} {
        regexp {^[0-9a-fA-F]$} $char
    }

    # Convert hex pair to byte value
    proc _hexPairToByte {hexPair} {
        scan $hexPair %x
    }

    # Convert byte to hex pair
    proc _byteToHexPair {byteValue} {
        format "%02x" $byteValue
    }

    # Parse UUID from canonical string format (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx)
    # Returns dict with {bytes ok error}
    proc parse {uuidString} {
        set trimmedInput [string trim $uuidString]

        # Check length (36 chars with hyphens)
        if {[string length $trimmedInput] != 36} {
            return [dict create bytes {} ok 0 \
                error "UUID must be 36 characters, got [string length $trimmedInput]"]
        }

        # Check hyphen positions (8, 13, 18, 23)
        if {[string index $trimmedInput 8] ne "-" ||
            [string index $trimmedInput 13] ne "-" ||
            [string index $trimmedInput 18] ne "-" ||
            [string index $trimmedInput 23] ne "-"} {
            return [dict create bytes {} ok 0 \
                error "Invalid UUID format: hyphens must be at positions 8, 13, 18, 23"]
        }

        # Remove hyphens and validate hex
        set hexString [string map {- ""} $trimmedInput]
        if {[string length $hexString] != 32} {
            return [dict create bytes {} ok 0 \
                error "Invalid UUID hex length"]
        }

        # Validate all hex characters
        if {![regexp {^[0-9a-fA-F]{32}$} $hexString]} {
            return [dict create bytes {} ok 0 \
                error "Invalid hex character in UUID"]
        }

        # Convert to bytes list
        set bytesList {}
        for {set i 0} {$i < 32} {incr i 2} {
            set hexPair [string range $hexString $i [expr {$i + 1}]]
            lappend bytesList [_hexPairToByte $hexPair]
        }

        return [dict create bytes $bytesList ok 1 error ""]
    }

    # Format bytes as canonical UUID string
    # Takes a bytes list (16 elements, 0-255 each)
    # Returns dict with {uuid ok error}
    proc format {bytesList} {
        if {[llength $bytesList] != 16} {
            return [dict create uuid "" ok 0 \
                error "UUID requires exactly 16 bytes"]
        }

        # Validate byte values
        foreach byteValue $bytesList {
            if {![string is integer -strict $byteValue] ||
                $byteValue < 0 || $byteValue > 255} {
                return [dict create uuid "" ok 0 \
                    error "Invalid byte value: $byteValue"]
            }
        }

        # Build canonical format: 8-4-4-4-12
        set hexParts {}
        foreach byteValue $bytesList {
            lappend hexParts [_byteToHexPair $byteValue]
        }

        set uuidString [join [lrange $hexParts 0 3] ""]
        append uuidString "-"
        append uuidString [join [lrange $hexParts 4 5] ""]
        append uuidString "-"
        append uuidString [join [lrange $hexParts 6 7] ""]
        append uuidString "-"
        append uuidString [join [lrange $hexParts 8 9] ""]
        append uuidString "-"
        append uuidString [join [lrange $hexParts 10 15] ""]

        return [dict create uuid $uuidString ok 1 error ""]
    }

    # Check if string is valid UUID format
    proc isValid {uuidString} {
        set result [parse $uuidString]
        return [dict get $result ok]
    }

    # Get UUID version from parsed bytes
    # Returns dict with {version ok error}
    proc version {bytesList} {
        if {[llength $bytesList] != 16} {
            return [dict create version 0 ok 0 \
                error "UUID requires exactly 16 bytes"]
        }

        # Version is in high nibble of byte 6
        set versionByte [lindex $bytesList 6]
        set versionNumber [expr {($versionByte >> 4) & 0x0F}]

        return [dict create version $versionNumber ok 1 error ""]
    }

    # Get UUID variant from parsed bytes
    # Returns dict with {variant ok error}
    proc variant {bytesList} {
        variable VARIANT_NCS
        variable VARIANT_RFC4122
        variable VARIANT_MICROSOFT
        variable VARIANT_FUTURE

        if {[llength $bytesList] != 16} {
            return [dict create variant 0 ok 0 \
                error "UUID requires exactly 16 bytes"]
        }

        # Variant is in high bits of byte 8
        set variantByte [lindex $bytesList 8]

        if {($variantByte >> 7) == 0} {
            set variantValue $VARIANT_NCS
        } elseif {($variantByte >> 6) == 0b10} {
            set variantValue $VARIANT_RFC4122
        } elseif {($variantByte >> 5) == 0b110} {
            set variantValue $VARIANT_MICROSOFT
        } else {
            set variantValue $VARIANT_FUTURE
        }

        return [dict create variant $variantValue ok 1 error ""]
    }

    # Check if bytes represent nil UUID
    proc isNil {bytesList} {
        if {[llength $bytesList] != 16} {
            return 0
        }

        foreach byteValue $bytesList {
            if {$byteValue != 0} {
                return 0
            }
        }
        return 1
    }

    # Create UUID from raw bytes list
    # Returns dict with {bytes ok error}
    proc fromBytes {bytesList} {
        if {[llength $bytesList] != 16} {
            return [dict create bytes {} ok 0 \
                error "UUID requires exactly 16 bytes"]
        }

        # Validate byte values
        foreach byteValue $bytesList {
            if {![string is integer -strict $byteValue] ||
                $byteValue < 0 || $byteValue > 255} {
                return [dict create bytes {} ok 0 \
                    error "Invalid byte value: $byteValue"]
            }
        }

        return [dict create bytes $bytesList ok 1 error ""]
    }

    # Format UUID as URN
    # Takes bytes list, returns dict with {urn ok error}
    proc toUrn {bytesList} {
        set formatResult [format $bytesList]
        if {![dict get $formatResult ok]} {
            return [dict create urn "" ok 0 \
                error [dict get $formatResult error]]
        }

        set uuidString [dict get $formatResult uuid]
        return [dict create urn "urn:uuid:$uuidString" ok 1 error ""]
    }

    # Generate v4 UUID from provided random bytes
    # Sets version to 4 and variant to RFC 4122
    # Returns dict with {bytes ok error}
    proc v4FromBytes {randomBytesList} {
        if {[llength $randomBytesList] != 16} {
            return [dict create bytes {} ok 0 \
                error "Requires exactly 16 random bytes"]
        }

        # Copy bytes
        set uuidBytes $randomBytesList

        # Set version to 4 (byte 6, high nibble)
        set byte6 [lindex $uuidBytes 6]
        set byte6 [expr {($byte6 & 0x0F) | 0x40}]
        lset uuidBytes 6 $byte6

        # Set variant to RFC 4122 (byte 8, high bits = 10)
        set byte8 [lindex $uuidBytes 8]
        set byte8 [expr {($byte8 & 0x3F) | 0x80}]
        lset uuidBytes 8 $byte8

        return [dict create bytes $uuidBytes ok 1 error ""]
    }
}
