# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# Proven - Safe, validated operations library for Tcl
# Version 0.4.0 - 38 modules
#

package provide proven 0.4.0
package require Tcl 8.6

# ============================================================
# SafeMath - Overflow-checked arithmetic
# ============================================================

namespace eval ::proven::math {
    namespace export safe_add safe_sub safe_mul safe_div safe_mod
    namespace export safe_abs safe_negate clamp in_range
    namespace export safe_pow safe_gcd safe_lcm

    # Maximum and minimum values for wide integers
    variable MAX_INT 9223372036854775807
    variable MIN_INT -9223372036854775808

    # Safe add with overflow checking
    # Returns dict with {value ok}
    proc safe_add {a b} {
        variable MAX_INT
        variable MIN_INT

        # Check for positive overflow: b > 0 and a > MAX - b
        if {$b > 0 && $a > $MAX_INT - $b} {
            return [dict create value 0 ok 0]
        }

        # Check for negative overflow: b < 0 and a < MIN - b
        if {$b < 0 && $a < $MIN_INT - $b} {
            return [dict create value 0 ok 0]
        }

        return [dict create value [expr {$a + $b}] ok 1]
    }

    # Safe subtract with overflow checking
    proc safe_sub {a b} {
        variable MAX_INT
        variable MIN_INT

        # Check for positive overflow: b < 0 and a > MAX + b
        if {$b < 0 && $a > $MAX_INT + $b} {
            return [dict create value 0 ok 0]
        }

        # Check for negative overflow: b > 0 and a < MIN + b
        if {$b > 0 && $a < $MIN_INT + $b} {
            return [dict create value 0 ok 0]
        }

        return [dict create value [expr {$a - $b}] ok 1]
    }

    # Safe multiply with overflow checking
    proc safe_mul {a b} {
        # Handle zero cases
        if {$a == 0 || $b == 0} {
            return [dict create value 0 ok 1]
        }

        set result [expr {$a * $b}]

        # Verify by division
        if {$a != 0 && $result / $a != $b} {
            return [dict create value 0 ok 0]
        }

        return [dict create value $result ok 1]
    }

    # Safe divide with zero check
    proc safe_div {a b} {
        variable MIN_INT

        if {$b == 0} {
            return [dict create value 0 ok 0]
        }

        # Check MIN / -1 overflow
        if {$a == $MIN_INT && $b == -1} {
            return [dict create value 0 ok 0]
        }

        return [dict create value [expr {$a / $b}] ok 1]
    }

    # Safe modulo with zero check
    proc safe_mod {a b} {
        if {$b == 0} {
            return [dict create value 0 ok 0]
        }

        return [dict create value [expr {$a % $b}] ok 1]
    }

    # Safe absolute value
    proc safe_abs {a} {
        variable MIN_INT

        if {$a == $MIN_INT} {
            return [dict create value 0 ok 0]
        }

        return [dict create value [expr {abs($a)}] ok 1]
    }

    # Safe negate
    proc safe_negate {a} {
        variable MIN_INT

        if {$a == $MIN_INT} {
            return [dict create value 0 ok 0]
        }

        return [dict create value [expr {-$a}] ok 1]
    }

    # Clamp value to range
    proc clamp {value min_val max_val} {
        if {$value < $min_val} {
            return $min_val
        } elseif {$value > $max_val} {
            return $max_val
        }
        return $value
    }

    # Check if value is in range
    proc in_range {value min_val max_val} {
        return [expr {$value >= $min_val && $value <= $max_val}]
    }

    # Safe power with overflow checking
    proc safe_pow {base exponent} {
        variable MAX_INT

        if {$exponent < 0} {
            return [dict create value 0 ok 0]
        }

        if {$exponent == 0} {
            return [dict create value 1 ok 1]
        }

        set result 1
        for {set i 0} {$i < $exponent} {incr i} {
            set mulResult [safe_mul $result $base]
            if {![dict get $mulResult ok]} {
                return [dict create value 0 ok 0]
            }
            set result [dict get $mulResult value]
        }

        return [dict create value $result ok 1]
    }

    # Greatest common divisor
    proc safe_gcd {a b} {
        set a [expr {abs($a)}]
        set b [expr {abs($b)}]

        while {$b != 0} {
            set temp $b
            set b [expr {$a % $b}]
            set a $temp
        }

        return [dict create value $a ok 1]
    }

    # Least common multiple
    proc safe_lcm {a b} {
        if {$a == 0 || $b == 0} {
            return [dict create value 0 ok 1]
        }

        set gcdResult [safe_gcd $a $b]
        set gcd [dict get $gcdResult value]

        set mulResult [safe_mul [expr {abs($a)}] [expr {abs($b)}]]
        if {![dict get $mulResult ok]} {
            return [dict create value 0 ok 0]
        }

        set product [dict get $mulResult value]
        return [dict create value [expr {$product / $gcd}] ok 1]
    }
}

# ============================================================
# SafeString - XSS prevention
# ============================================================

namespace eval ::proven::string {
    namespace export escape_html escape_sql escape_js sanitize_default
    namespace export url_encode url_decode slugify truncate_safe
    namespace export is_ascii is_printable normalize_whitespace

    # Escape HTML special characters
    proc escape_html {input} {
        set result $input
        set result [string map {
            & "&amp;"
            < "&lt;"
            > "&gt;"
            \" "&quot;"
            ' "&#x27;"
        } $result]
        return $result
    }

    # Escape SQL single quotes
    proc escape_sql {input} {
        return [string map {' ''} $input]
    }

    # Escape JavaScript special characters
    proc escape_js {input} {
        set result $input
        set result [string map {
            \\ "\\\\"
            \" "\\\""
            ' "\\'"
            \n "\\n"
            \r "\\r"
            \t "\\t"
            < "\\u003C"
            > "\\u003E"
            / "\\/"
        } $result]
        return $result
    }

    # Sanitize to alphanumeric + underscore + hyphen
    proc sanitize_default {input} {
        regsub -all {[^a-zA-Z0-9_-]} $input {} result
        return $result
    }

    # URL encode
    proc url_encode {input} {
        set result ""
        foreach char [split $input ""] {
            if {[regexp {[a-zA-Z0-9._~-]} $char]} {
                append result $char
            } else {
                binary scan $char H2 hex
                append result "%[string toupper $hex]"
            }
        }
        return $result
    }

    # URL decode
    proc url_decode {input} {
        set result ""
        set i 0
        set len [string length $input]

        while {$i < $len} {
            set char [string index $input $i]
            if {$char eq "%" && $i + 2 < $len} {
                set hex [string range $input [expr {$i + 1}] [expr {$i + 2}]]
                if {[regexp {^[0-9a-fA-F]{2}$} $hex]} {
                    append result [format %c [scan $hex %x]]
                    incr i 3
                    continue
                }
            } elseif {$char eq "+"} {
                append result " "
                incr i
                continue
            }
            append result $char
            incr i
        }
        return $result
    }

    # Convert to URL-safe slug
    proc slugify {input} {
        set result [string tolower $input]
        # Replace non-alphanumeric with hyphens
        regsub -all {[^a-z0-9]+} $result "-" result
        # Remove leading/trailing hyphens
        set result [string trim $result "-"]
        return $result
    }

    # Safely truncate string with ellipsis
    proc truncate_safe {input maxLen {suffix "..."}} {
        if {[string length $input] <= $maxLen} {
            return $input
        }
        set suffixLen [string length $suffix]
        if {$maxLen <= $suffixLen} {
            return [string range $suffix 0 [expr {$maxLen - 1}]]
        }
        return "[string range $input 0 [expr {$maxLen - $suffixLen - 1}]]$suffix"
    }

    # Check if string is ASCII only
    proc is_ascii {input} {
        foreach char [split $input ""] {
            scan $char %c code
            if {$code > 127} {
                return 0
            }
        }
        return 1
    }

    # Check if string is printable ASCII
    proc is_printable {input} {
        foreach char [split $input ""] {
            scan $char %c code
            if {$code < 32 || $code > 126} {
                return 0
            }
        }
        return 1
    }

    # Normalize whitespace
    proc normalize_whitespace {input} {
        set result [string trim $input]
        regsub -all {\s+} $result " " result
        return $result
    }
}

# ============================================================
# SafePath - Directory traversal prevention
# ============================================================

namespace eval ::proven::path {
    namespace export has_traversal sanitize_filename safe_path_join
    namespace export is_safe_extension get_extension normalize_path

    # Check for path traversal patterns
    proc has_traversal {path} {
        set lower_path [string tolower $path]

        # Check for ".."
        if {[string first ".." $path] >= 0} {
            return 1
        }

        # Check for "./"
        if {[string first "./" $path] >= 0} {
            return 1
        }

        # Check for URL-encoded ".." (%2e%2e)
        if {[string first "%2e%2e" $lower_path] >= 0} {
            return 1
        }

        # Check for null byte (%00)
        if {[string first "%00" $lower_path] >= 0} {
            return 1
        }

        return 0
    }

    # Sanitize filename
    proc sanitize_filename {input} {
        # Replace dangerous characters with underscore
        regsub -all {[/\\:*?"<>|]} $input "_" result
        return $result
    }

    # Safely join paths
    # Returns dict with {path error ok}
    proc safe_path_join {base filename} {
        # Check for traversal
        if {[has_traversal $filename]} {
            return [dict create path "" error "Path traversal detected" ok 0]
        }

        # Sanitize filename
        set safe_name [sanitize_filename $filename]

        # Join paths
        if {[string index $base end] eq "/"} {
            set path "${base}${safe_name}"
        } else {
            set path "${base}/${safe_name}"
        }

        return [dict create path $path error "" ok 1]
    }

    # Check if extension is in safe list
    proc is_safe_extension {filename safeExtensions} {
        set ext [get_extension $filename]
        set lowerExt [string tolower $ext]
        foreach safe $safeExtensions {
            if {$lowerExt eq [string tolower $safe]} {
                return 1
            }
        }
        return 0
    }

    # Get file extension
    proc get_extension {filename} {
        set idx [string last "." $filename]
        if {$idx < 0} {
            return ""
        }
        return [string range $filename [expr {$idx + 1}] end]
    }

    # Normalize path (remove double slashes, etc.)
    proc normalize_path {path} {
        # Remove double slashes
        regsub -all {//+} $path "/" result
        # Remove trailing slash (except for root)
        if {[string length $result] > 1 && [string index $result end] eq "/"} {
            set result [string range $result 0 end-1]
        }
        return $result
    }
}

# ============================================================
# SafeEmail - Email validation
# ============================================================

namespace eval ::proven::email {
    namespace export is_valid_email parse_email is_disposable_email
    namespace export normalize_email get_domain mask_email

    variable MAX_EMAIL_LEN 254
    variable MAX_LOCAL_LEN 64
    variable MAX_DOMAIN_LEN 255

    variable DISPOSABLE_DOMAINS {
        mailinator.com
        guerrillamail.com
        10minutemail.com
        tempmail.com
        throwaway.email
        fakeinbox.com
        trashmail.com
        maildrop.cc
        yopmail.com
        temp-mail.org
    }

    # Basic email validation
    proc is_valid_email {email} {
        variable MAX_EMAIL_LEN
        variable MAX_LOCAL_LEN

        set email_len [string length $email]
        if {$email_len == 0 || $email_len > $MAX_EMAIL_LEN} {
            return 0
        }

        # Find @
        set at_pos [string first "@" $email]
        if {$at_pos < 0} {
            return 0
        }

        # Must have exactly one @
        if {[string first "@" $email [expr {$at_pos + 1}]] >= 0} {
            return 0
        }

        # @ cannot be first or last
        if {$at_pos == 0 || $at_pos == $email_len - 1} {
            return 0
        }

        # Check local part length
        if {$at_pos > $MAX_LOCAL_LEN} {
            return 0
        }

        # Domain must have at least one dot
        set domain [string range $email [expr {$at_pos + 1}] end]
        if {[string first "." $domain] < 0} {
            return 0
        }

        return 1
    }

    # Parse email into local part and domain
    # Returns dict with {local_part domain error ok}
    proc parse_email {email} {
        variable MAX_EMAIL_LEN
        variable MAX_LOCAL_LEN

        set email_len [string length $email]
        if {$email_len == 0} {
            return [dict create local_part "" domain "" error "Email is empty" ok 0]
        }

        if {$email_len > $MAX_EMAIL_LEN} {
            return [dict create local_part "" domain "" error "Email exceeds maximum length" ok 0]
        }

        set at_pos [string first "@" $email]
        if {$at_pos < 0} {
            return [dict create local_part "" domain "" error "Missing @ symbol" ok 0]
        }

        if {$at_pos == 0} {
            return [dict create local_part "" domain "" error "Local part is empty" ok 0]
        }

        if {$at_pos > $MAX_LOCAL_LEN} {
            return [dict create local_part "" domain "" error "Local part exceeds maximum length" ok 0]
        }

        set local_part [string range $email 0 [expr {$at_pos - 1}]]
        set domain [string range $email [expr {$at_pos + 1}] end]

        if {[string length $domain] == 0} {
            return [dict create local_part "" domain "" error "Domain is empty" ok 0]
        }

        if {[string first "." $domain] < 0} {
            return [dict create local_part "" domain "" error "Domain must contain a dot" ok 0]
        }

        return [dict create local_part $local_part domain $domain error "" ok 1]
    }

    # Check for disposable email domain
    proc is_disposable_email {domain} {
        variable DISPOSABLE_DOMAINS

        set lower_domain [string tolower $domain]
        foreach d $DISPOSABLE_DOMAINS {
            if {$lower_domain eq $d} {
                return 1
            }
        }
        return 0
    }

    # Normalize email (lowercase domain)
    proc normalize_email {email} {
        set at_pos [string first "@" $email]
        if {$at_pos < 0} {
            return $email
        }

        set local_part [string range $email 0 [expr {$at_pos - 1}]]
        set domain [string tolower [string range $email [expr {$at_pos + 1}] end]]

        return "${local_part}@${domain}"
    }

    # Get domain from email
    proc get_domain {email} {
        set at_pos [string first "@" $email]
        if {$at_pos < 0} {
            return ""
        }
        return [string range $email [expr {$at_pos + 1}] end]
    }

    # Mask email for display (user@example.com -> u***@e***.com)
    proc mask_email {email} {
        set parseResult [parse_email $email]
        if {![dict get $parseResult ok]} {
            return $email
        }

        set local [dict get $parseResult local_part]
        set domain [dict get $parseResult domain]

        # Mask local part
        if {[string length $local] <= 2} {
            set maskedLocal "${local}***"
        } else {
            set maskedLocal "[string index $local 0]***"
        }

        # Mask domain
        set dotPos [string first "." $domain]
        if {$dotPos > 0} {
            set domainName [string range $domain 0 [expr {$dotPos - 1}]]
            set tld [string range $domain $dotPos end]
            if {[string length $domainName] <= 2} {
                set maskedDomain "${domainName}***${tld}"
            } else {
                set maskedDomain "[string index $domainName 0]***${tld}"
            }
        } else {
            set maskedDomain $domain
        }

        return "${maskedLocal}@${maskedDomain}"
    }
}

# ============================================================
# SafeNetwork - IP validation
# ============================================================

namespace eval ::proven::network {
    namespace export parse_ipv4 format_ipv4 is_loopback is_private_ip
    namespace export is_reserved_ip is_public_ip classify_ip
    namespace export is_valid_port is_privileged_port
    namespace export parse_cidr ip_in_cidr

    # IP classification constants
    variable IP_CLASS_INVALID  0
    variable IP_CLASS_LOOPBACK 1
    variable IP_CLASS_PRIVATE  2
    variable IP_CLASS_RESERVED 3
    variable IP_CLASS_PUBLIC   4

    # Parse IPv4 address
    # Returns dict with {octets valid}
    proc parse_ipv4 {address} {
        set octets [split $address "."]

        if {[llength $octets] != 4} {
            return [dict create octets {0 0 0 0} valid 0]
        }

        set result {}
        foreach octet $octets {
            # Check for empty
            if {$octet eq ""} {
                return [dict create octets {0 0 0 0} valid 0]
            }

            # Check for leading zeros
            if {[string length $octet] > 1 && [string index $octet 0] eq "0"} {
                return [dict create octets {0 0 0 0} valid 0]
            }

            # Check numeric
            if {![string is integer -strict $octet]} {
                return [dict create octets {0 0 0 0} valid 0]
            }

            # Check range
            if {$octet < 0 || $octet > 255} {
                return [dict create octets {0 0 0 0} valid 0]
            }

            lappend result $octet
        }

        return [dict create octets $result valid 1]
    }

    # Format IPv4 address
    proc format_ipv4 {ip_dict} {
        set octets [dict get $ip_dict octets]
        return [join $octets "."]
    }

    # Check if IP is loopback
    proc is_loopback {ip_dict} {
        set octets [dict get $ip_dict octets]
        return [expr {[lindex $octets 0] == 127}]
    }

    # Check if IP is private (RFC 1918)
    proc is_private_ip {ip_dict} {
        set octets [dict get $ip_dict octets]
        set o1 [lindex $octets 0]
        set o2 [lindex $octets 1]

        # 10.0.0.0/8
        if {$o1 == 10} {
            return 1
        }

        # 172.16.0.0/12
        if {$o1 == 172 && $o2 >= 16 && $o2 <= 31} {
            return 1
        }

        # 192.168.0.0/16
        if {$o1 == 192 && $o2 == 168} {
            return 1
        }

        return 0
    }

    # Check if IP is reserved
    proc is_reserved_ip {ip_dict} {
        set octets [dict get $ip_dict octets]
        set o1 [lindex $octets 0]
        set o2 [lindex $octets 1]

        # 0.0.0.0/8
        if {$o1 == 0} { return 1 }

        # 100.64.0.0/10 (CGNAT)
        if {$o1 == 100 && $o2 >= 64 && $o2 <= 127} { return 1 }

        # 169.254.0.0/16 (link-local)
        if {$o1 == 169 && $o2 == 254} { return 1 }

        # 224-239.x.x.x (multicast)
        if {$o1 >= 224 && $o1 <= 239} { return 1 }

        # 240+ (reserved/broadcast)
        if {$o1 >= 240} { return 1 }

        return 0
    }

    # Check if IP is public
    proc is_public_ip {ip_dict} {
        if {![dict get $ip_dict valid]} { return 0 }
        if {[is_loopback $ip_dict]} { return 0 }
        if {[is_private_ip $ip_dict]} { return 0 }
        if {[is_reserved_ip $ip_dict]} { return 0 }
        return 1
    }

    # Classify IP address
    proc classify_ip {ip_dict} {
        variable IP_CLASS_INVALID
        variable IP_CLASS_LOOPBACK
        variable IP_CLASS_PRIVATE
        variable IP_CLASS_RESERVED
        variable IP_CLASS_PUBLIC

        if {![dict get $ip_dict valid]} {
            return $IP_CLASS_INVALID
        }
        if {[is_loopback $ip_dict]} {
            return $IP_CLASS_LOOPBACK
        }
        if {[is_private_ip $ip_dict]} {
            return $IP_CLASS_PRIVATE
        }
        if {[is_reserved_ip $ip_dict]} {
            return $IP_CLASS_RESERVED
        }
        return $IP_CLASS_PUBLIC
    }

    # Check if port is valid
    proc is_valid_port {port} {
        return [expr {$port >= 1 && $port <= 65535}]
    }

    # Check if port is privileged
    proc is_privileged_port {port} {
        return [expr {$port >= 1 && $port < 1024}]
    }

    # Parse CIDR notation
    # Returns dict with {ip prefix ok error}
    proc parse_cidr {cidr} {
        set slashPos [string first "/" $cidr]
        if {$slashPos < 0} {
            return [dict create ip {} prefix 0 ok 0 error "Missing / in CIDR"]
        }

        set ipPart [string range $cidr 0 [expr {$slashPos - 1}]]
        set prefixPart [string range $cidr [expr {$slashPos + 1}] end]

        set ipResult [parse_ipv4 $ipPart]
        if {![dict get $ipResult valid]} {
            return [dict create ip {} prefix 0 ok 0 error "Invalid IP address"]
        }

        if {![string is integer -strict $prefixPart]} {
            return [dict create ip {} prefix 0 ok 0 error "Invalid prefix length"]
        }

        if {$prefixPart < 0 || $prefixPart > 32} {
            return [dict create ip {} prefix 0 ok 0 error "Prefix must be 0-32"]
        }

        return [dict create ip $ipResult prefix $prefixPart ok 1 error ""]
    }

    # Check if IP is in CIDR range
    proc ip_in_cidr {ip_dict cidr} {
        set cidrResult [parse_cidr $cidr]
        if {![dict get $cidrResult ok]} {
            return 0
        }

        if {![dict get $ip_dict valid]} {
            return 0
        }

        set ipOctets [dict get $ip_dict octets]
        set netOctets [dict get [dict get $cidrResult ip] octets]
        set prefix [dict get $cidrResult prefix]

        # Convert to 32-bit integers
        set ipInt [expr {([lindex $ipOctets 0] << 24) | ([lindex $ipOctets 1] << 16) | ([lindex $ipOctets 2] << 8) | [lindex $ipOctets 3]}]
        set netInt [expr {([lindex $netOctets 0] << 24) | ([lindex $netOctets 1] << 16) | ([lindex $netOctets 2] << 8) | [lindex $netOctets 3]}]

        # Create mask
        set mask [expr {$prefix == 0 ? 0 : (0xFFFFFFFF << (32 - $prefix)) & 0xFFFFFFFF}]

        return [expr {($ipInt & $mask) == ($netInt & $mask)}]
    }
}

# ============================================================
# SafeCrypto - Cryptographic operations
# ============================================================

namespace eval ::proven::crypto {
    namespace export constant_time_equals simple_hash bytes_to_hex
    namespace export generate_token random_int secure_wipe
    namespace export hex_to_bytes xor_bytes

    variable HEX_CHARS "0123456789abcdef"
    variable TOKEN_CHARS "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

    # Constant-time comparison
    proc constant_time_equals {a b} {
        if {[string length $a] != [string length $b]} {
            return 0
        }

        set diff 0
        for {set i 0} {$i < [string length $a]} {incr i} {
            scan [string index $a $i] %c ca
            scan [string index $b $i] %c cb
            set diff [expr {$diff | ($ca ^ $cb)}]
        }

        return [expr {$diff == 0}]
    }

    # Simple hash (demo only - NOT cryptographically secure)
    proc simple_hash {input} {
        set h 0

        for {set i 0} {$i < [string length $input]} {incr i} {
            scan [string index $input $i] %c c
            set h [expr {(($h * 33) + $c) & 0xFFFFFFFF}]
        }

        return [format "%08x" $h]
    }

    # Convert bytes to hex
    proc bytes_to_hex {data} {
        binary scan $data H* hex
        return $hex
    }

    # Convert hex to bytes list
    proc hex_to_bytes {hex} {
        set result {}
        set len [string length $hex]
        for {set i 0} {$i < $len} {incr i 2} {
            set byte [string range $hex $i [expr {$i + 1}]]
            lappend result [scan $byte %x]
        }
        return $result
    }

    # XOR two byte lists
    proc xor_bytes {a b} {
        set result {}
        set len [expr {min([llength $a], [llength $b])}]
        for {set i 0} {$i < $len} {incr i} {
            lappend result [expr {[lindex $a $i] ^ [lindex $b $i]}]
        }
        return $result
    }

    # Generate random token
    proc generate_token {length} {
        variable TOKEN_CHARS

        set result ""
        set chars_len [string length $TOKEN_CHARS]

        for {set i 0} {$i < $length} {incr i} {
            set idx [expr {int(rand() * $chars_len)}]
            append result [string index $TOKEN_CHARS $idx]
        }

        return $result
    }

    # Random integer in range
    proc random_int {min_val max_val} {
        set range [expr {$max_val - $min_val + 1}]
        return [expr {$min_val + int(rand() * $range)}]
    }

    # Secure wipe (best effort in Tcl)
    proc secure_wipe {varname} {
        upvar $varname var

        set len [string length $var]

        # Overwrite with zeros
        set var [string repeat "\x00" $len]

        # Overwrite with ones
        set var [string repeat "\xff" $len]

        # Final zeros
        set var [string repeat "\x00" $len]
    }
}

# ============================================================
# Main namespace exports
# ============================================================

namespace eval ::proven {
    namespace export math string path email network crypto
    namespace export uuid currency phone hex
    namespace export url json datetime float version color angle unit
    namespace export buffer queue bloom lru graph
    namespace export rate_limiter circuit_breaker retry monotonic
    namespace export state_machine calculator
    namespace export geo probability checksum tensor
    namespace export password ml
    namespace export header cookie content_type

    # Library metadata
    variable VERSION "0.4.0"
    variable MODULE_COUNT 38

    proc version {} {
        variable VERSION
        return $VERSION
    }

    proc module_count {} {
        variable MODULE_COUNT
        return $MODULE_COUNT
    }
}
