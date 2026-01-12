# Proven for Tcl

Safe, validated operations library for Tcl applications.

## Installation

Add to your Tcl package path:

```tcl
lappend auto_path /path/to/proven/bindings/tcl
package require proven
```

Or source directly:

```tcl
source /path/to/proven/bindings/tcl/proven.tcl
```

## Usage

```tcl
package require proven

# Import namespaces for convenience
namespace import ::proven::math::*
namespace import ::proven::string::*
namespace import ::proven::path::*
namespace import ::proven::email::*
namespace import ::proven::network::*
namespace import ::proven::crypto::*

# Safe math with overflow checking
set result [safe_add 1000000000 2000000000]
if {[dict get $result ok]} {
    puts "Sum: [dict get $result value]"
} else {
    puts "Overflow!"
}

# XSS prevention
set safe_html [escape_html "<script>alert('xss')</script>"]
# "&lt;script&gt;alert('xss')&lt;/script&gt;"

# Path safety
if {[has_traversal "../../../etc/passwd"]} {
    error "Dangerous path!"
}

# Email validation
set email_result [parse_email "user@example.com"]
if {[dict get $email_result ok]} {
    puts "Local: [dict get $email_result local_part]"
    puts "Domain: [dict get $email_result domain]"
} else {
    puts "Invalid: [dict get $email_result error]"
}

# IP classification
set ip [parse_ipv4 "192.168.1.1"]
if {[dict get $ip valid]} {
    puts "Private: [is_private_ip $ip]"
    puts "Classification: [classify_ip $ip]"
}

# Secure tokens
set token [generate_token 32]
puts "Token: $token"
```

## Modules

### ::proven::math

Overflow-checked arithmetic:

```tcl
namespace import ::proven::math::*

# All operations return dict with {value ok}
set result [safe_add $a $b]
set result [safe_sub $a $b]
set result [safe_mul $a $b]
set result [safe_div $a $b]
set result [safe_mod $a $b]

set result [safe_abs $a]
set result [safe_negate $a]

# Check result
if {[dict get $result ok]} {
    puts [dict get $result value]
} else {
    puts "Operation failed!"
}

# Range operations
set clamped [clamp $value $min $max]
set is_in [in_range $value $min $max]
```

### ::proven::string

XSS prevention and string sanitization:

```tcl
namespace import ::proven::string::*

escape_html "<script>"       ;# "&lt;script&gt;"
escape_sql "O'Brien"         ;# "O''Brien"
escape_js "line\nbreak"      ;# "line\\nbreak"
url_encode "hello world"     ;# "hello%20world"
sanitize_default "user@host" ;# "userhost"
slugify "Hello World!"       ;# "hello-world"
```

### ::proven::path

Directory traversal protection:

```tcl
namespace import ::proven::path::*

# Check for traversal
if {[has_traversal $user_path]} {
    error "Path traversal detected"
}

# Sanitize filename
set safe_name [sanitize_filename "../bad<file>.txt"]

# Safe path joining - returns dict with {path error ok}
set result [safe_path_join "/base" "file.txt"]
if {[dict get $result ok]} {
    puts "Path: [dict get $result path]"
} else {
    puts "Error: [dict get $result error]"
}
```

### ::proven::email

Email validation:

```tcl
namespace import ::proven::email::*

# Simple validation
if {[is_valid_email $email]} {
    puts "Valid format"
}

# Parse with result checking - returns dict
set result [parse_email $email]
if {[dict get $result ok]} {
    puts "Local: [dict get $result local_part]"
    puts "Domain: [dict get $result domain]"
} else {
    puts "Error: [dict get $result error]"
}

# Check for disposable emails
if {[is_disposable_email "mailinator.com"]} {
    puts "Disposable domain"
}

# Normalize (lowercase domain)
set normalized [normalize_email "User@EXAMPLE.COM"]
# "User@example.com"
```

### ::proven::network

IP address validation and classification:

```tcl
namespace import ::proven::network::*

# Parse IPv4 - returns dict with {octets valid}
set ip [parse_ipv4 "192.168.1.1"]
if {[dict get $ip valid]} {
    puts "Octets: [dict get $ip octets]"

    # Type checks
    puts "Loopback: [is_loopback $ip]"
    puts "Private: [is_private_ip $ip]"
    puts "Reserved: [is_reserved_ip $ip]"
    puts "Public: [is_public_ip $ip]"

    # Classification (returns integer constant)
    set class [classify_ip $ip]
    switch $class {
        1 { puts "Loopback" }
        2 { puts "Private" }
        3 { puts "Reserved" }
        4 { puts "Public" }
    }
}

# Format as string
set ip_str [format_ipv4 $ip]

# Port validation
is_valid_port 8080      ;# 1 (true)
is_privileged_port 80   ;# 1 (true)
```

**IP Classification Constants:**
```tcl
$::proven::network::IP_CLASS_INVALID  ;# 0
$::proven::network::IP_CLASS_LOOPBACK ;# 1
$::proven::network::IP_CLASS_PRIVATE  ;# 2
$::proven::network::IP_CLASS_RESERVED ;# 3
$::proven::network::IP_CLASS_PUBLIC   ;# 4
```

### ::proven::crypto

Cryptographic operations:

```tcl
namespace import ::proven::crypto::*

# Constant-time comparison (timing attack prevention)
if {[constant_time_equals $actual $expected]} {
    puts "Match"
}

# Simple hash (demo only - NOT cryptographically secure)
set hash [simple_hash "data"]

# Convert bytes to hex
set hex [bytes_to_hex $binary_data]

# Generate random token
set token [generate_token 32]

# Random integer in range
set val [random_int 1 100]

# Secure wipe (pass variable name)
set sensitive "secret data"
secure_wipe sensitive
```

**Note:** The crypto operations are simplified for demonstration. For production applications, use Tcl's `tls` package or external cryptographic libraries.

## Return Types

### Safe Operation Result

```tcl
# Math operations return:
dict create value <integer> ok <boolean>

# Path operations return:
dict create path <string> error <string> ok <boolean>

# Email operations return:
dict create local_part <string> domain <string> error <string> ok <boolean>

# IP parsing returns:
dict create octets <list> valid <boolean>
```

## Examples

### Safe Input Processing

```tcl
package require proven

proc process_user_input {name email path} {
    namespace import ::proven::string::*
    namespace import ::proven::email::*
    namespace import ::proven::path::*

    # Sanitize name for display
    set safe_name [escape_html $name]

    # Validate email
    set email_result [parse_email $email]
    if {![dict get $email_result ok]} {
        error "Invalid email: [dict get $email_result error]"
    }

    # Check path is safe
    if {[has_traversal $path]} {
        error "Invalid path"
    }

    return [list \
        name $safe_name \
        email [normalize_email $email] \
        path [sanitize_filename $path] \
    ]
}

# Usage
set result [process_user_input \
    "<script>Bob</script>" \
    "user@EXAMPLE.COM" \
    "../file.txt" \
]
```

### Network Request Validation

```tcl
package require proven

proc validate_request {ip_addr port} {
    namespace import ::proven::network::*

    # Parse and validate IP
    set ip [parse_ipv4 $ip_addr]
    if {![dict get $ip valid]} {
        return [dict create ok 0 error "Invalid IP address"]
    }

    # Block private IPs (SSRF protection)
    if {[is_private_ip $ip] || [is_loopback $ip]} {
        return [dict create ok 0 error "Private IPs not allowed"]
    }

    # Validate port
    if {![is_valid_port $port]} {
        return [dict create ok 0 error "Invalid port"]
    }

    return [dict create ok 1 ip $ip_addr port $port]
}
```

### Safe Arithmetic in Financial Context

```tcl
package require proven

proc calculate_total {items} {
    namespace import ::proven::math::*

    set total 0
    foreach item $items {
        set price [dict get $item price]
        set qty [dict get $item quantity]

        # Safe multiplication
        set subtotal [safe_mul $price $qty]
        if {![dict get $subtotal ok]} {
            error "Overflow in subtotal calculation"
        }

        # Safe addition
        set new_total [safe_add $total [dict get $subtotal value]]
        if {![dict get $new_total ok]} {
            error "Overflow in total calculation"
        }

        set total [dict get $new_total value]
    }

    return $total
}
```

## Compatibility

This library requires:
- Tcl 8.6 or later
- No external dependencies

Compatible with:
- Tcl/Tk 8.6+
- Jim Tcl (partial support)
- Tcl embedded in applications

## License

PMPL-1.0
