# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""Proven Safety Primitives for Starlark/Bazel.

Provides type-safe value constructors and validation functions
for use in Bazel BUILD files and .bzl extensions.

This module implements all 38 Proven safety modules:

Core (11): safe_math, safe_string, safe_path, safe_email, safe_url,
           safe_network, safe_crypto, safe_uuid, safe_currency, safe_phone, safe_hex

Data (7): safe_json, safe_datetime, safe_float, safe_version, safe_color,
          safe_angle, safe_unit

Data Structures (5): safe_buffer, safe_queue, safe_bloom, safe_lru, safe_graph

Resilience (4): safe_rate_limiter, safe_circuit_breaker, safe_retry, safe_monotonic

State (2): safe_state_machine, safe_calculator

Algorithm (4): safe_geo, safe_probability, safe_checksum, safe_tensor

Security (2): safe_password, safe_ml

HTTP (3): safe_header, safe_cookie, safe_content_type
"""

# Version and module count
VERSION = "0.4.0"
MODULE_COUNT = 38

# Module categories for documentation
MODULE_CATEGORIES = {
    "core": [
        "safe_math",
        "safe_string",
        "safe_path",
        "safe_email",
        "safe_url",
        "safe_network",
        "safe_crypto",
        "safe_uuid",
        "safe_currency",
        "safe_phone",
        "safe_hex",
    ],
    "data": [
        "safe_json",
        "safe_datetime",
        "safe_float",
        "safe_version",
        "safe_color",
        "safe_angle",
        "safe_unit",
    ],
    "data_structures": [
        "safe_buffer",
        "safe_queue",
        "safe_bloom",
        "safe_lru",
        "safe_graph",
    ],
    "resilience": [
        "safe_rate_limiter",
        "safe_circuit_breaker",
        "safe_retry",
        "safe_monotonic",
    ],
    "state": [
        "safe_state_machine",
        "safe_calculator",
    ],
    "algorithm": [
        "safe_geo",
        "safe_probability",
        "safe_checksum",
        "safe_tensor",
    ],
    "security": [
        "safe_password",
        "safe_ml",
    ],
    "http": [
        "safe_header",
        "safe_cookie",
        "safe_content_type",
    ],
}

# ============================================================================
# RESULT TYPE
# ============================================================================

def ok(value):
    """Create a successful Result.

    Args:
        value: The success value to wrap.

    Returns:
        A Result dict with ok=True and the wrapped value.
    """
    return {"ok": True, "value": value}

def err(error):
    """Create an error Result.

    Args:
        error: The error message or value.

    Returns:
        A Result dict with ok=False and the error.
    """
    return {"ok": False, "error": error}

def is_ok(result):
    """Check if Result is Ok.

    Args:
        result: A Result dict.

    Returns:
        True if the result is Ok, False otherwise.
    """
    return result.get("ok", False)

def is_err(result):
    """Check if Result is Err.

    Args:
        result: A Result dict.

    Returns:
        True if the result is Err, False otherwise.
    """
    return not result.get("ok", True)

def unwrap(result):
    """Unwrap Result value, failing if Err.

    Args:
        result: A Result dict.

    Returns:
        The unwrapped value.

    Fails:
        If the result is an Err.
    """
    if is_ok(result):
        return result["value"]
    fail("Unwrap on Err: " + str(result.get("error", "unknown")))

def unwrap_or(result, default):
    """Unwrap Result value or return default.

    Args:
        result: A Result dict.
        default: The default value to return if Err.

    Returns:
        The unwrapped value or the default.
    """
    return result["value"] if is_ok(result) else default

def map_result(result, fn):
    """Transform Result value if Ok.

    Args:
        result: A Result dict.
        fn: A function to apply to the value.

    Returns:
        A new Result with the transformed value, or the original error.
    """
    if is_ok(result):
        return ok(fn(result["value"]))
    return result

def and_then(result, fn):
    """Chain Result operations (flatMap).

    Args:
        result: A Result dict.
        fn: A function that returns a Result.

    Returns:
        The result of fn if Ok, or the original error.
    """
    if is_ok(result):
        return fn(result["value"])
    return result

def or_else(result, fn):
    """Provide fallback for Err.

    Args:
        result: A Result dict.
        fn: A function to call if Err.

    Returns:
        The original result if Ok, or the result of fn.
    """
    if is_ok(result):
        return result
    return fn(result["error"])

# ============================================================================
# SAFE_MATH: Core arithmetic with overflow protection
# ============================================================================

# Integer bounds for 64-bit signed integers
_INT64_MAX = 9223372036854775807
_INT64_MIN = -9223372036854775808

def safe_add(a, b):
    """Safe addition with overflow checking.

    Args:
        a: First operand.
        b: Second operand.

    Returns:
        Result with sum or overflow error.
    """
    result = a + b
    if result > _INT64_MAX or result < _INT64_MIN:
        return err("Integer overflow in addition")
    return ok(result)

def safe_sub(a, b):
    """Safe subtraction with overflow checking.

    Args:
        a: First operand.
        b: Second operand.

    Returns:
        Result with difference or overflow error.
    """
    result = a - b
    if result > _INT64_MAX or result < _INT64_MIN:
        return err("Integer overflow in subtraction")
    return ok(result)

def safe_mul(a, b):
    """Safe multiplication with overflow checking.

    Args:
        a: First operand.
        b: Second operand.

    Returns:
        Result with product or overflow error.
    """
    if a == 0 or b == 0:
        return ok(0)
    result = a * b
    if result // a != b:
        return err("Integer overflow in multiplication")
    return ok(result)

def safe_div(a, b):
    """Safe division (no divide by zero).

    Args:
        a: Dividend.
        b: Divisor.

    Returns:
        Result with quotient or division by zero error.
    """
    if b == 0:
        return err("Division by zero")
    return ok(a // b)

def safe_mod(a, b):
    """Safe modulo (no divide by zero).

    Args:
        a: Dividend.
        b: Divisor.

    Returns:
        Result with remainder or division by zero error.
    """
    if b == 0:
        return err("Division by zero")
    return ok(a % b)

def safe_pow(base, exp):
    """Safe exponentiation with bounds checking.

    Args:
        base: The base number.
        exp: The exponent (must be non-negative).

    Returns:
        Result with power or error.
    """
    if exp < 0:
        return err("Negative exponent not supported")
    if exp == 0:
        return ok(1)
    result = 1
    for _ in range(exp):
        result = result * base
        if result > _INT64_MAX or result < _INT64_MIN:
            return err("Integer overflow in exponentiation")
    return ok(result)

def clamp(value, min_val, max_val):
    """Clamp value to range [min, max].

    Args:
        value: The value to clamp.
        min_val: Minimum allowed value.
        max_val: Maximum allowed value.

    Returns:
        The clamped value.
    """
    return max(min_val, min(max_val, value))

def abs_safe(n):
    """Safe absolute value.

    Args:
        n: The number.

    Returns:
        Result with absolute value or overflow error.
    """
    if n == _INT64_MIN:
        return err("Overflow: abs of INT64_MIN")
    return ok(abs(n))

# ============================================================================
# SAFE_STRING: String validation and manipulation
# ============================================================================

def validate_non_empty(s, field_name = "string"):
    """Validate string is non-empty.

    Args:
        s: The string to validate.
        field_name: Name of the field for error messages.

    Returns:
        Result with the string or error.
    """
    if not s:
        return err("Field '{}' cannot be empty".format(field_name))
    return ok(s)

def validate_max_length(s, max_len, field_name = "string"):
    """Validate string length does not exceed maximum.

    Args:
        s: The string to validate.
        max_len: Maximum allowed length.
        field_name: Name of the field for error messages.

    Returns:
        Result with the string or error.
    """
    if len(s) > max_len:
        return err("Field '{}' exceeds max length {} (got {})".format(
            field_name,
            max_len,
            len(s),
        ))
    return ok(s)

def validate_min_length(s, min_len, field_name = "string"):
    """Validate string length meets minimum.

    Args:
        s: The string to validate.
        min_len: Minimum required length.
        field_name: Name of the field for error messages.

    Returns:
        Result with the string or error.
    """
    if len(s) < min_len:
        return err("Field '{}' below min length {} (got {})".format(
            field_name,
            min_len,
            len(s),
        ))
    return ok(s)

def validate_alphanumeric(s, field_name = "string"):
    """Validate string contains only alphanumeric characters.

    Args:
        s: The string to validate.
        field_name: Name of the field for error messages.

    Returns:
        Result with the string or error.
    """
    for c in s.elems():
        if not (c.isalpha() or c.isdigit()):
            return err("Field '{}' must be alphanumeric".format(field_name))
    return ok(s)

def safe_trim(s):
    """Safely trim whitespace from string.

    Args:
        s: The string to trim.

    Returns:
        Result with trimmed string.
    """
    return ok(s.strip())

def safe_lower(s):
    """Safely convert string to lowercase.

    Args:
        s: The string to convert.

    Returns:
        Result with lowercase string.
    """
    return ok(s.lower())

def safe_upper(s):
    """Safely convert string to uppercase.

    Args:
        s: The string to convert.

    Returns:
        Result with uppercase string.
    """
    return ok(s.upper())

# ============================================================================
# SAFE_PATH: Path validation and construction
# ============================================================================

def validate_path_component(component):
    """Validate a single path component.

    Args:
        component: The path component to validate.

    Returns:
        Result with the component or error.
    """
    if not component:
        return err("Path component cannot be empty")
    if ".." in component:
        return err("Path component cannot contain '..'")
    if "/" in component or "\\" in component:
        return err("Path component cannot contain path separators")
    return ok(component)

def validate_relative_path(path):
    """Validate a relative path (no leading slash).

    Args:
        path: The path to validate.

    Returns:
        Result with the path or error.
    """
    if not path:
        return err("Path cannot be empty")
    if path.startswith("/"):
        return err("Relative path cannot start with '/'")
    if ".." in path.split("/"):
        return err("Path cannot contain '..' components")
    return ok(path)

def validate_absolute_path(path):
    """Validate an absolute path.

    Args:
        path: The path to validate.

    Returns:
        Result with the path or error.
    """
    if not path:
        return err("Path cannot be empty")
    if not path.startswith("/"):
        return err("Absolute path must start with '/'")
    if ".." in path.split("/"):
        return err("Path cannot contain '..' components")
    return ok(path)

def safe_join_path(*components):
    """Safely join path components.

    Args:
        *components: Path components to join.

    Returns:
        Result with joined path or error.
    """
    validated = []
    for i, comp in enumerate(components):
        if i == 0 and comp.startswith("/"):
            validated.append(comp.rstrip("/"))
        else:
            result = validate_path_component(comp)
            if is_err(result):
                return result
            validated.append(comp)
    return ok("/".join(validated))

def get_extension(path):
    """Get file extension from path.

    Args:
        path: The file path.

    Returns:
        Result with extension (including dot) or error if no extension.
    """
    if "." not in path.split("/")[-1]:
        return err("No file extension found")
    parts = path.rsplit(".", 1)
    return ok("." + parts[-1])

def get_basename(path):
    """Get basename from path.

    Args:
        path: The file path.

    Returns:
        Result with basename.
    """
    return ok(path.split("/")[-1])

def get_dirname(path):
    """Get directory name from path.

    Args:
        path: The file path.

    Returns:
        Result with directory name.
    """
    parts = path.rsplit("/", 1)
    if len(parts) == 1:
        return ok(".")
    return ok(parts[0] if parts[0] else "/")

# ============================================================================
# SAFE_EMAIL: Email address validation
# ============================================================================

def validate_email(email):
    """Validate email address format.

    Args:
        email: The email address to validate.

    Returns:
        Result with email or error.
    """
    if not email:
        return err("Email cannot be empty")
    if email.count("@") != 1:
        return err("Email must contain exactly one '@'")
    local, domain = email.split("@")
    if not local:
        return err("Email local part cannot be empty")
    if not domain:
        return err("Email domain cannot be empty")
    if "." not in domain:
        return err("Email domain must contain at least one '.'")
    if domain.startswith(".") or domain.endswith("."):
        return err("Email domain cannot start or end with '.'")
    if ".." in domain:
        return err("Email domain cannot contain consecutive dots")
    return ok(email)

def get_email_domain(email):
    """Extract domain from email address.

    Args:
        email: The email address.

    Returns:
        Result with domain or error.
    """
    result = validate_email(email)
    if is_err(result):
        return result
    return ok(email.split("@")[1])

def get_email_local_part(email):
    """Extract local part from email address.

    Args:
        email: The email address.

    Returns:
        Result with local part or error.
    """
    result = validate_email(email)
    if is_err(result):
        return result
    return ok(email.split("@")[0])

# ============================================================================
# SAFE_URL: URL validation and parsing
# ============================================================================

# Common URL schemes
VALID_URL_SCHEMES = ["http", "https", "ftp", "ssh", "git", "file"]

def validate_url_scheme(scheme):
    """Validate URL scheme.

    Args:
        scheme: The URL scheme.

    Returns:
        Result with scheme or error.
    """
    if scheme.lower() not in VALID_URL_SCHEMES:
        return err("Invalid URL scheme: " + scheme)
    return ok(scheme.lower())

def parse_url(url):
    """Parse URL into components.

    Args:
        url: The URL string.

    Returns:
        Result with URL components dict or error.
    """
    if not url:
        return err("URL cannot be empty")

    # Check for scheme
    if "://" not in url:
        return err("URL must contain scheme (e.g., https://)")

    scheme, rest = url.split("://", 1)
    scheme_result = validate_url_scheme(scheme)
    if is_err(scheme_result):
        return scheme_result

    # Extract path, query, fragment
    path = "/"
    query = ""
    fragment = ""

    if "#" in rest:
        rest, fragment = rest.split("#", 1)
    if "?" in rest:
        rest, query = rest.split("?", 1)
    if "/" in rest:
        host_port, path = rest.split("/", 1)
        path = "/" + path
    else:
        host_port = rest

    # Extract port
    port = None
    host = host_port
    if ":" in host_port:
        parts = host_port.rsplit(":", 1)
        if parts[1].isdigit():
            host = parts[0]
            port = int(parts[1])

    return ok({
        "scheme": scheme.lower(),
        "host": host,
        "port": port,
        "path": path,
        "query": query,
        "fragment": fragment,
    })

def validate_url(url):
    """Validate URL format.

    Args:
        url: The URL to validate.

    Returns:
        Result with URL or error.
    """
    result = parse_url(url)
    if is_err(result):
        return result
    return ok(url)

def build_url(scheme, host, port = None, path = "/", query = "", fragment = ""):
    """Build URL from components.

    Args:
        scheme: URL scheme (http, https, etc.).
        host: Host name.
        port: Optional port number.
        path: Path component.
        query: Query string (without ?).
        fragment: Fragment (without #).

    Returns:
        Result with URL string or error.
    """
    scheme_result = validate_url_scheme(scheme)
    if is_err(scheme_result):
        return scheme_result

    if not host:
        return err("Host cannot be empty")

    url = scheme + "://" + host
    if port:
        url += ":" + str(port)
    url += path
    if query:
        url += "?" + query
    if fragment:
        url += "#" + fragment

    return ok(url)

# ============================================================================
# SAFE_NETWORK: Network-related validations
# ============================================================================

def validate_port(port):
    """Validate port number is in valid range (1-65535).

    Args:
        port: The port number.

    Returns:
        Result with port or error.
    """
    if type(port) != "int":
        return err("Port must be an integer, got: " + type(port))
    if port < 1 or port > 65535:
        return err("Port must be between 1 and 65535, got: " + str(port))
    return ok(port)

def validate_ipv4(ip):
    """Validate IPv4 address format.

    Args:
        ip: The IPv4 address string.

    Returns:
        Result with IP or error.
    """
    if not ip:
        return err("IP address cannot be empty")
    parts = ip.split(".")
    if len(parts) != 4:
        return err("IPv4 must have 4 octets")
    for part in parts:
        if not part.isdigit():
            return err("IPv4 octet must be numeric: " + part)
        octet = int(part)
        if octet < 0 or octet > 255:
            return err("IPv4 octet must be 0-255: " + part)
    return ok(ip)

def validate_hostname(hostname):
    """Validate hostname format.

    Args:
        hostname: The hostname string.

    Returns:
        Result with hostname or error.
    """
    if not hostname:
        return err("Hostname cannot be empty")
    if len(hostname) > 253:
        return err("Hostname too long (max 253 characters)")
    if hostname.startswith("-") or hostname.endswith("-"):
        return err("Hostname cannot start or end with hyphen")
    if hostname.startswith(".") or hostname.endswith("."):
        return err("Hostname cannot start or end with dot")

    labels = hostname.split(".")
    for label in labels:
        if not label:
            return err("Hostname cannot contain empty labels")
        if len(label) > 63:
            return err("Hostname label too long (max 63 characters)")
        for c in label.elems():
            if not (c.isalnum() or c == "-"):
                return err("Invalid character in hostname: " + c)
    return ok(hostname)

def validate_cidr(cidr):
    """Validate CIDR notation.

    Args:
        cidr: CIDR string (e.g., "192.168.1.0/24").

    Returns:
        Result with CIDR or error.
    """
    if "/" not in cidr:
        return err("CIDR must contain '/'")
    ip, prefix = cidr.rsplit("/", 1)

    ip_result = validate_ipv4(ip)
    if is_err(ip_result):
        return ip_result

    if not prefix.isdigit():
        return err("CIDR prefix must be numeric")
    prefix_int = int(prefix)
    if prefix_int < 0 or prefix_int > 32:
        return err("CIDR prefix must be 0-32")

    return ok(cidr)

# Common ports
COMMON_PORTS = struct(
    http = 80,
    https = 443,
    ssh = 22,
    dns = 53,
    mysql = 3306,
    postgres = 5432,
    redis = 6379,
    grpc = 50051,
    mongodb = 27017,
    elasticsearch = 9200,
)

# ============================================================================
# SAFE_CRYPTO: Cryptographic constant definitions
# ============================================================================

# Supported hash algorithms
HASH_ALGORITHMS = ["sha256", "sha384", "sha512", "sha3-256", "sha3-512", "blake2b", "blake3"]

# Key sizes in bits
KEY_SIZES = struct(
    aes_128 = 128,
    aes_192 = 192,
    aes_256 = 256,
    rsa_2048 = 2048,
    rsa_3072 = 3072,
    rsa_4096 = 4096,
    ed25519 = 256,
    x25519 = 256,
)

def validate_hash_algorithm(algorithm):
    """Validate hash algorithm name.

    Args:
        algorithm: The algorithm name.

    Returns:
        Result with algorithm or error.
    """
    if algorithm.lower() not in HASH_ALGORITHMS:
        return err("Unsupported hash algorithm: {}. Supported: {}".format(
            algorithm,
            ", ".join(HASH_ALGORITHMS),
        ))
    return ok(algorithm.lower())

def validate_key_size(size, algorithm_type):
    """Validate key size for algorithm type.

    Args:
        size: Key size in bits.
        algorithm_type: Type of algorithm ("aes", "rsa", "ed25519").

    Returns:
        Result with size or error.
    """
    valid_sizes = {
        "aes": [128, 192, 256],
        "rsa": [2048, 3072, 4096],
        "ed25519": [256],
        "x25519": [256],
    }
    if algorithm_type not in valid_sizes:
        return err("Unknown algorithm type: " + algorithm_type)
    if size not in valid_sizes[algorithm_type]:
        return err("Invalid key size {} for {}. Valid: {}".format(
            size,
            algorithm_type,
            valid_sizes[algorithm_type],
        ))
    return ok(size)

def validate_hex_hash(hash_str, expected_length = None):
    """Validate hexadecimal hash string.

    Args:
        hash_str: The hash string.
        expected_length: Expected length in characters (optional).

    Returns:
        Result with hash or error.
    """
    if not hash_str:
        return err("Hash string cannot be empty")
    for c in hash_str.lower().elems():
        if c not in "0123456789abcdef":
            return err("Invalid hexadecimal character: " + c)
    if expected_length and len(hash_str) != expected_length:
        return err("Hash length {} does not match expected {}".format(
            len(hash_str),
            expected_length,
        ))
    return ok(hash_str.lower())

# ============================================================================
# SAFE_UUID: UUID validation
# ============================================================================

def validate_uuid(uuid_str):
    """Validate UUID format.

    Args:
        uuid_str: The UUID string.

    Returns:
        Result with UUID or error.
    """
    if not uuid_str:
        return err("UUID cannot be empty")

    # Remove optional braces
    s = uuid_str
    if s.startswith("{") and s.endswith("}"):
        s = s[1:-1]

    # Check format: 8-4-4-4-12
    parts = s.split("-")
    if len(parts) != 5:
        return err("UUID must have 5 parts separated by hyphens")

    expected_lengths = [8, 4, 4, 4, 12]
    for i, (part, expected) in enumerate(zip(parts, expected_lengths)):
        if len(part) != expected:
            return err("UUID part {} has wrong length: {} (expected {})".format(
                i + 1,
                len(part),
                expected,
            ))
        for c in part.lower().elems():
            if c not in "0123456789abcdef":
                return err("UUID contains invalid character: " + c)

    return ok(s.lower())

def uuid_version(uuid_str):
    """Extract UUID version.

    Args:
        uuid_str: A valid UUID string.

    Returns:
        Result with version number (1-5) or error.
    """
    result = validate_uuid(uuid_str)
    if is_err(result):
        return result

    s = unwrap(result)
    version_char = s.split("-")[2][0]
    if version_char in "12345":
        return ok(int(version_char))
    return err("Unknown UUID version: " + version_char)

# ============================================================================
# SAFE_CURRENCY: Currency validation
# ============================================================================

# ISO 4217 currency codes (subset)
ISO_CURRENCY_CODES = [
    "USD", "EUR", "GBP", "JPY", "CNY", "CHF", "CAD", "AUD", "NZD",
    "HKD", "SGD", "SEK", "NOK", "DKK", "INR", "BRL", "MXN", "KRW",
]

def validate_currency_code(code):
    """Validate ISO 4217 currency code.

    Args:
        code: The currency code.

    Returns:
        Result with code or error.
    """
    if not code:
        return err("Currency code cannot be empty")
    upper = code.upper()
    if len(upper) != 3:
        return err("Currency code must be 3 characters")
    if upper not in ISO_CURRENCY_CODES:
        return err("Unknown currency code: " + upper)
    return ok(upper)

def validate_currency_amount(amount, allow_negative = False):
    """Validate currency amount.

    Args:
        amount: The amount (integer in minor units, e.g., cents).
        allow_negative: Whether negative amounts are allowed.

    Returns:
        Result with amount or error.
    """
    if type(amount) != "int":
        return err("Currency amount must be integer (minor units)")
    if not allow_negative and amount < 0:
        return err("Currency amount cannot be negative")
    return ok(amount)

def format_currency(amount_minor, code, decimal_places = 2):
    """Format currency amount for display.

    Args:
        amount_minor: Amount in minor units (e.g., cents).
        code: Currency code.
        decimal_places: Number of decimal places.

    Returns:
        Result with formatted string or error.
    """
    code_result = validate_currency_code(code)
    if is_err(code_result):
        return code_result

    divisor = 1
    for _ in range(decimal_places):
        divisor *= 10

    whole = amount_minor // divisor
    frac = abs(amount_minor) % divisor
    frac_str = str(frac).zfill(decimal_places)

    return ok("{} {}.{}".format(code, whole, frac_str))

# ============================================================================
# SAFE_PHONE: Phone number validation
# ============================================================================

def validate_phone_e164(phone):
    """Validate phone number in E.164 format.

    Args:
        phone: The phone number (e.g., "+14155551234").

    Returns:
        Result with phone or error.
    """
    if not phone:
        return err("Phone number cannot be empty")
    if not phone.startswith("+"):
        return err("E.164 phone must start with '+'")
    digits = phone[1:]
    if not digits.isdigit():
        return err("Phone number must contain only digits after '+'")
    if len(digits) < 7 or len(digits) > 15:
        return err("E.164 phone must be 7-15 digits")
    return ok(phone)

def validate_phone_nanp(phone):
    """Validate North American phone number (NANP).

    Args:
        phone: The phone number.

    Returns:
        Result with phone or error.
    """
    # Strip common formatting
    digits = ""
    for c in phone.elems():
        if c.isdigit():
            digits += c

    if len(digits) == 11 and digits.startswith("1"):
        digits = digits[1:]

    if len(digits) != 10:
        return err("NANP phone must be 10 digits")

    # NPA (area code) cannot start with 0 or 1
    if digits[0] in "01":
        return err("Area code cannot start with 0 or 1")

    # NXX (exchange) cannot start with 0 or 1
    if digits[3] in "01":
        return err("Exchange cannot start with 0 or 1")

    return ok("+1" + digits)

# ============================================================================
# SAFE_HEX: Hexadecimal validation and conversion
# ============================================================================

def validate_hex(s):
    """Validate hexadecimal string.

    Args:
        s: The string to validate.

    Returns:
        Result with lowercase hex string or error.
    """
    if not s:
        return err("Hex string cannot be empty")
    for c in s.lower().elems():
        if c not in "0123456789abcdef":
            return err("Invalid hex character: " + c)
    return ok(s.lower())

def hex_to_int(hex_str):
    """Convert hex string to integer.

    Args:
        hex_str: The hex string (without 0x prefix).

    Returns:
        Result with integer or error.
    """
    result = validate_hex(hex_str)
    if is_err(result):
        return result

    value = 0
    for c in hex_str.lower().elems():
        value = value * 16
        if c.isdigit():
            value += int(c)
        else:
            value += ord(c) - ord("a") + 10
    return ok(value)

def int_to_hex(n, min_width = 0):
    """Convert integer to hex string.

    Args:
        n: The integer (must be non-negative).
        min_width: Minimum width with zero padding.

    Returns:
        Result with hex string or error.
    """
    if n < 0:
        return err("Cannot convert negative number to hex")

    if n == 0:
        hex_str = "0"
    else:
        hex_str = ""
        val = n
        while val > 0:
            digit = val % 16
            if digit < 10:
                hex_str = str(digit) + hex_str
            else:
                hex_str = chr(ord("a") + digit - 10) + hex_str
            val = val // 16

    while len(hex_str) < min_width:
        hex_str = "0" + hex_str

    return ok(hex_str)

# ============================================================================
# SAFE_JSON: JSON validation helpers
# ============================================================================

def validate_json_key(key):
    """Validate JSON object key.

    Args:
        key: The key string.

    Returns:
        Result with key or error.
    """
    if type(key) != "string":
        return err("JSON key must be a string")
    if not key:
        return err("JSON key cannot be empty")
    return ok(key)

def validate_json_type(value, expected_type):
    """Validate JSON value type.

    Args:
        value: The value to check.
        expected_type: Expected type ("string", "int", "bool", "list", "dict", "None").

    Returns:
        Result with value or error.
    """
    actual = type(value)
    type_map = {
        "string": "string",
        "int": "int",
        "bool": "bool",
        "list": "list",
        "dict": "dict",
        "None": "NoneType",
    }
    expected_actual = type_map.get(expected_type, expected_type)
    if actual != expected_actual:
        return err("Expected {} but got {}".format(expected_type, actual))
    return ok(value)

def json_get(obj, key, default = None):
    """Safely get value from JSON object.

    Args:
        obj: The dict object.
        key: The key to get.
        default: Default value if key not found.

    Returns:
        Result with value or default.
    """
    if type(obj) != "dict":
        return err("Cannot get key from non-dict")
    return ok(obj.get(key, default))

def json_get_string(obj, key, default = ""):
    """Safely get string value from JSON object.

    Args:
        obj: The dict object.
        key: The key to get.
        default: Default value if key not found.

    Returns:
        Result with string value or error.
    """
    result = json_get(obj, key, default)
    if is_err(result):
        return result
    value = unwrap(result)
    if type(value) != "string":
        return err("Key '{}' is not a string".format(key))
    return ok(value)

def json_get_int(obj, key, default = 0):
    """Safely get integer value from JSON object.

    Args:
        obj: The dict object.
        key: The key to get.
        default: Default value if key not found.

    Returns:
        Result with integer value or error.
    """
    result = json_get(obj, key, default)
    if is_err(result):
        return result
    value = unwrap(result)
    if type(value) != "int":
        return err("Key '{}' is not an integer".format(key))
    return ok(value)

# ============================================================================
# SAFE_DATETIME: Date and time validation
# ============================================================================

def validate_iso_date(date_str):
    """Validate ISO 8601 date format (YYYY-MM-DD).

    Args:
        date_str: The date string.

    Returns:
        Result with date components or error.
    """
    if not date_str:
        return err("Date string cannot be empty")

    parts = date_str.split("-")
    if len(parts) != 3:
        return err("Date must be in YYYY-MM-DD format")

    year_str, month_str, day_str = parts

    if len(year_str) != 4 or not year_str.isdigit():
        return err("Year must be 4 digits")
    if len(month_str) != 2 or not month_str.isdigit():
        return err("Month must be 2 digits")
    if len(day_str) != 2 or not day_str.isdigit():
        return err("Day must be 2 digits")

    year = int(year_str)
    month = int(month_str)
    day = int(day_str)

    if month < 1 or month > 12:
        return err("Month must be 1-12")

    days_in_month = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    if year % 4 == 0 and (year % 100 != 0 or year % 400 == 0):
        days_in_month[2] = 29

    if day < 1 or day > days_in_month[month]:
        return err("Invalid day for month")

    return ok({"year": year, "month": month, "day": day})

def validate_iso_time(time_str):
    """Validate ISO 8601 time format (HH:MM:SS).

    Args:
        time_str: The time string.

    Returns:
        Result with time components or error.
    """
    if not time_str:
        return err("Time string cannot be empty")

    # Handle optional timezone
    tz = None
    base_time = time_str
    if "Z" in time_str:
        base_time = time_str.replace("Z", "")
        tz = "Z"
    elif "+" in time_str[1:]:
        idx = time_str.rfind("+")
        base_time = time_str[:idx]
        tz = time_str[idx:]
    elif time_str.count("-") > 0 and time_str.rfind("-") > 2:
        idx = time_str.rfind("-")
        base_time = time_str[:idx]
        tz = time_str[idx:]

    parts = base_time.split(":")
    if len(parts) < 2 or len(parts) > 3:
        return err("Time must be in HH:MM or HH:MM:SS format")

    hour_str = parts[0]
    minute_str = parts[1]
    second_str = parts[2] if len(parts) > 2 else "00"

    # Handle fractional seconds
    if "." in second_str:
        second_str = second_str.split(".")[0]

    if len(hour_str) != 2 or not hour_str.isdigit():
        return err("Hour must be 2 digits")
    if len(minute_str) != 2 or not minute_str.isdigit():
        return err("Minute must be 2 digits")
    if len(second_str) != 2 or not second_str.isdigit():
        return err("Second must be 2 digits")

    hour = int(hour_str)
    minute = int(minute_str)
    second = int(second_str)

    if hour < 0 or hour > 23:
        return err("Hour must be 0-23")
    if minute < 0 or minute > 59:
        return err("Minute must be 0-59")
    if second < 0 or second > 59:
        return err("Second must be 0-59")

    return ok({"hour": hour, "minute": minute, "second": second, "timezone": tz})

def validate_timestamp(timestamp):
    """Validate Unix timestamp.

    Args:
        timestamp: The timestamp (seconds since epoch).

    Returns:
        Result with timestamp or error.
    """
    if type(timestamp) != "int":
        return err("Timestamp must be an integer")
    if timestamp < 0:
        return err("Timestamp cannot be negative")
    if timestamp > 253402300799:  # Year 9999
        return err("Timestamp too large")
    return ok(timestamp)

# ============================================================================
# SAFE_FLOAT: Floating-point validation
# ============================================================================

def validate_float_range(f, min_val, max_val, field_name = "value"):
    """Validate float is within range.

    Args:
        f: The float value.
        min_val: Minimum allowed value.
        max_val: Maximum allowed value.
        field_name: Name for error messages.

    Returns:
        Result with float or error.
    """
    if type(f) not in ["int", "float"]:
        return err("Field '{}' must be numeric".format(field_name))
    if f < min_val or f > max_val:
        return err("Field '{}' must be between {} and {}".format(
            field_name,
            min_val,
            max_val,
        ))
    return ok(f)

def validate_percentage(p, field_name = "percentage"):
    """Validate percentage (0-100).

    Args:
        p: The percentage value.
        field_name: Name for error messages.

    Returns:
        Result with percentage or error.
    """
    return validate_float_range(p, 0, 100, field_name)

def validate_probability(p, field_name = "probability"):
    """Validate probability (0-1).

    Args:
        p: The probability value.
        field_name: Name for error messages.

    Returns:
        Result with probability or error.
    """
    return validate_float_range(p, 0, 1, field_name)

def validate_positive_float(f, field_name = "value"):
    """Validate float is positive.

    Args:
        f: The float value.
        field_name: Name for error messages.

    Returns:
        Result with float or error.
    """
    if type(f) not in ["int", "float"]:
        return err("Field '{}' must be numeric".format(field_name))
    if f <= 0:
        return err("Field '{}' must be positive".format(field_name))
    return ok(f)

# ============================================================================
# SAFE_VERSION: Semantic versioning
# ============================================================================

def parse_semver(version_str):
    """Parse semantic version string.

    Args:
        version_str: Version string (e.g., "1.2.3", "1.2.3-beta.1+build.123").

    Returns:
        Result with version dict or error.
    """
    if not version_str:
        return err("Version string cannot be empty")

    # Remove optional 'v' prefix
    s = version_str
    if s.startswith("v") or s.startswith("V"):
        s = s[1:]

    # Split off build metadata
    build_metadata = ""
    if "+" in s:
        s, build_metadata = s.split("+", 1)

    # Split off prerelease
    prerelease = ""
    if "-" in s:
        s, prerelease = s.split("-", 1)

    # Parse major.minor.patch
    parts = s.split(".")
    if len(parts) < 1 or len(parts) > 3:
        return err("Version must have 1-3 numeric parts")

    for i, part in enumerate(parts):
        if not part.isdigit():
            return err("Version part {} must be numeric".format(i + 1))

    major = int(parts[0])
    minor = int(parts[1]) if len(parts) > 1 else 0
    patch = int(parts[2]) if len(parts) > 2 else 0

    return ok({
        "major": major,
        "minor": minor,
        "patch": patch,
        "prerelease": prerelease,
        "build_metadata": build_metadata,
    })

def compare_semver(v1, v2):
    """Compare two semantic versions.

    Args:
        v1: First version dict.
        v2: Second version dict.

    Returns:
        -1 if v1 < v2, 0 if equal, 1 if v1 > v2.
    """
    if v1["major"] != v2["major"]:
        return -1 if v1["major"] < v2["major"] else 1
    if v1["minor"] != v2["minor"]:
        return -1 if v1["minor"] < v2["minor"] else 1
    if v1["patch"] != v2["patch"]:
        return -1 if v1["patch"] < v2["patch"] else 1

    # Prerelease comparison (presence means lower precedence)
    if v1["prerelease"] and not v2["prerelease"]:
        return -1
    if not v1["prerelease"] and v2["prerelease"]:
        return 1
    if v1["prerelease"] < v2["prerelease"]:
        return -1
    if v1["prerelease"] > v2["prerelease"]:
        return 1

    return 0

def validate_version_constraint(constraint):
    """Validate version constraint string.

    Args:
        constraint: Constraint string (e.g., ">=1.0.0", "^1.2.3", "~1.2").

    Returns:
        Result with parsed constraint or error.
    """
    if not constraint:
        return err("Constraint cannot be empty")

    operators = [">=", "<=", "!=", ">", "<", "=", "^", "~"]
    op = ""
    version_part = constraint

    for o in operators:
        if constraint.startswith(o):
            op = o
            version_part = constraint[len(o):]
            break

    if not op:
        op = "="

    version_result = parse_semver(version_part)
    if is_err(version_result):
        return version_result

    return ok({"operator": op, "version": unwrap(version_result)})

# ============================================================================
# SAFE_COLOR: Color validation and conversion
# ============================================================================

def validate_hex_color(color):
    """Validate hexadecimal color code.

    Args:
        color: Color string (e.g., "#FF0000", "FF0000", "#F00").

    Returns:
        Result with normalized 6-digit hex color or error.
    """
    s = color
    if s.startswith("#"):
        s = s[1:]

    if len(s) not in [3, 6]:
        return err("Hex color must be 3 or 6 digits")

    for c in s.lower().elems():
        if c not in "0123456789abcdef":
            return err("Invalid hex color character: " + c)

    # Expand 3-digit to 6-digit
    if len(s) == 3:
        s = s[0] + s[0] + s[1] + s[1] + s[2] + s[2]

    return ok("#" + s.upper())

def validate_rgb(r, g, b):
    """Validate RGB color values.

    Args:
        r: Red component (0-255).
        g: Green component (0-255).
        b: Blue component (0-255).

    Returns:
        Result with RGB dict or error.
    """
    for name, val in [("red", r), ("green", g), ("blue", b)]:
        if type(val) != "int":
            return err("{} must be an integer".format(name))
        if val < 0 or val > 255:
            return err("{} must be 0-255, got {}".format(name, val))
    return ok({"r": r, "g": g, "b": b})

def rgb_to_hex(r, g, b):
    """Convert RGB to hex color.

    Args:
        r: Red component (0-255).
        g: Green component (0-255).
        b: Blue component (0-255).

    Returns:
        Result with hex color string or error.
    """
    result = validate_rgb(r, g, b)
    if is_err(result):
        return result

    r_hex = unwrap(int_to_hex(r, 2))
    g_hex = unwrap(int_to_hex(g, 2))
    b_hex = unwrap(int_to_hex(b, 2))

    return ok("#" + r_hex.upper() + g_hex.upper() + b_hex.upper())

def hex_to_rgb(hex_color):
    """Convert hex color to RGB.

    Args:
        hex_color: Hex color string.

    Returns:
        Result with RGB dict or error.
    """
    result = validate_hex_color(hex_color)
    if is_err(result):
        return result

    s = unwrap(result)[1:]  # Remove #

    r = unwrap(hex_to_int(s[0:2]))
    g = unwrap(hex_to_int(s[2:4]))
    b = unwrap(hex_to_int(s[4:6]))

    return ok({"r": r, "g": g, "b": b})

# ============================================================================
# SAFE_ANGLE: Angle validation and conversion
# ============================================================================

def validate_degrees(degrees):
    """Validate degrees (0-360).

    Args:
        degrees: Angle in degrees.

    Returns:
        Result with normalized degrees (0-360) or error.
    """
    if type(degrees) not in ["int", "float"]:
        return err("Degrees must be numeric")
    # Normalize to 0-360
    normalized = degrees % 360
    if normalized < 0:
        normalized += 360
    return ok(normalized)

def validate_radians(radians):
    """Validate radians.

    Args:
        radians: Angle in radians.

    Returns:
        Result with radians or error.
    """
    if type(radians) not in ["int", "float"]:
        return err("Radians must be numeric")
    return ok(radians)

def degrees_to_radians(degrees):
    """Convert degrees to radians.

    Args:
        degrees: Angle in degrees.

    Returns:
        Result with radians or error.
    """
    result = validate_degrees(degrees)
    if is_err(result):
        return result
    # PI approximation for Starlark
    pi = 3.141592653589793
    return ok(unwrap(result) * pi / 180.0)

def radians_to_degrees(radians):
    """Convert radians to degrees.

    Args:
        radians: Angle in radians.

    Returns:
        Result with degrees or error.
    """
    result = validate_radians(radians)
    if is_err(result):
        return result
    pi = 3.141592653589793
    return ok(unwrap(result) * 180.0 / pi)

# ============================================================================
# SAFE_UNIT: Unit conversion and validation
# ============================================================================

# Memory units
MEMORY_UNITS = {
    "B": 1,
    "KB": 1024,
    "KiB": 1024,
    "MB": 1024 * 1024,
    "MiB": 1024 * 1024,
    "GB": 1024 * 1024 * 1024,
    "GiB": 1024 * 1024 * 1024,
    "TB": 1024 * 1024 * 1024 * 1024,
    "TiB": 1024 * 1024 * 1024 * 1024,
}

# Time units in milliseconds
TIME_UNITS_MS = {
    "ms": 1,
    "s": 1000,
    "sec": 1000,
    "m": 60000,
    "min": 60000,
    "h": 3600000,
    "hour": 3600000,
    "d": 86400000,
    "day": 86400000,
}

def memory_bytes(value, unit):
    """Convert memory to bytes.

    Args:
        value: Numeric value.
        unit: One of "B", "KB", "MB", "GB", "TB" (or IEC variants).

    Returns:
        Result with bytes value or error.
    """
    if unit not in MEMORY_UNITS:
        return err("Unknown memory unit: {}. Valid: {}".format(
            unit,
            ", ".join(sorted(MEMORY_UNITS.keys())),
        ))
    if value <= 0:
        return err("Memory must be positive")
    return ok(value * MEMORY_UNITS[unit])

def bytes_to_human(bytes_val):
    """Convert bytes to human-readable string.

    Args:
        bytes_val: Number of bytes.

    Returns:
        Result with human-readable string or error.
    """
    if bytes_val < 0:
        return err("Bytes cannot be negative")

    units = ["B", "KB", "MB", "GB", "TB"]
    val = bytes_val
    for unit in units:
        if val < 1024:
            return ok("{} {}".format(int(val), unit))
        val = val / 1024
    return ok("{} TB".format(int(val * 1024)))

def time_milliseconds(value, unit):
    """Convert time to milliseconds.

    Args:
        value: Numeric value.
        unit: One of "ms", "s", "m", "h", "d".

    Returns:
        Result with milliseconds value or error.
    """
    if unit not in TIME_UNITS_MS:
        return err("Unknown time unit: {}. Valid: {}".format(
            unit,
            ", ".join(sorted(TIME_UNITS_MS.keys())),
        ))
    if value < 0:
        return err("Time cannot be negative")
    return ok(int(value * TIME_UNITS_MS[unit]))

def cpu_millicores(value, unit = "cores"):
    """Convert CPU to millicores.

    Args:
        value: Numeric value.
        unit: One of "m", "millicores", "cores", "" (default: cores).

    Returns:
        Result with millicores value or error.
    """
    units = {
        "m": 1,
        "millicores": 1,
        "cores": 1000,
        "": 1000,
    }
    if unit not in units:
        return err("Unknown CPU unit: " + unit)
    if value <= 0:
        return err("CPU must be positive")
    return ok(int(value * units[unit]))

# ============================================================================
# SAFE_BUFFER: Buffer/byte array operations
# ============================================================================

def create_buffer(size, fill_value = 0):
    """Create a buffer (list) of specified size.

    Args:
        size: Number of elements.
        fill_value: Value to fill with (default 0).

    Returns:
        Result with buffer list or error.
    """
    if size < 0:
        return err("Buffer size cannot be negative")
    if size > 1048576:  # 1MB limit for Starlark
        return err("Buffer size exceeds maximum (1MB)")
    return ok([fill_value] * size)

def buffer_get(buf, index):
    """Safely get buffer element.

    Args:
        buf: The buffer list.
        index: Index to get.

    Returns:
        Result with element or error.
    """
    if index < 0 or index >= len(buf):
        return err("Buffer index out of bounds: {} (size: {})".format(index, len(buf)))
    return ok(buf[index])

def buffer_slice(buf, start, end):
    """Safely slice buffer.

    Args:
        buf: The buffer list.
        start: Start index (inclusive).
        end: End index (exclusive).

    Returns:
        Result with slice or error.
    """
    if start < 0:
        start = 0
    if end > len(buf):
        end = len(buf)
    if start > end:
        return err("Start index cannot exceed end index")
    return ok(buf[start:end])

# ============================================================================
# SAFE_QUEUE: Queue operations
# ============================================================================

def create_queue(max_size = 0):
    """Create a queue structure.

    Args:
        max_size: Maximum queue size (0 for unlimited).

    Returns:
        Queue dict structure.
    """
    return {"items": [], "max_size": max_size}

def queue_push(queue, item):
    """Push item to queue.

    Args:
        queue: The queue dict.
        item: Item to push.

    Returns:
        Result with updated queue or error.
    """
    if queue["max_size"] > 0 and len(queue["items"]) >= queue["max_size"]:
        return err("Queue is full (max: {})".format(queue["max_size"]))
    return ok({"items": queue["items"] + [item], "max_size": queue["max_size"]})

def queue_pop(queue):
    """Pop item from queue (FIFO).

    Args:
        queue: The queue dict.

    Returns:
        Result with (item, updated_queue) tuple or error.
    """
    if not queue["items"]:
        return err("Queue is empty")
    item = queue["items"][0]
    new_queue = {"items": queue["items"][1:], "max_size": queue["max_size"]}
    return ok((item, new_queue))

def queue_peek(queue):
    """Peek at front item without removing.

    Args:
        queue: The queue dict.

    Returns:
        Result with front item or error.
    """
    if not queue["items"]:
        return err("Queue is empty")
    return ok(queue["items"][0])

def queue_size(queue):
    """Get queue size.

    Args:
        queue: The queue dict.

    Returns:
        Number of items in queue.
    """
    return len(queue["items"])

def queue_is_empty(queue):
    """Check if queue is empty.

    Args:
        queue: The queue dict.

    Returns:
        True if empty, False otherwise.
    """
    return len(queue["items"]) == 0

# ============================================================================
# SAFE_BLOOM: Bloom filter approximation (limited in Starlark)
# ============================================================================

def create_bloom_filter(size, num_hashes = 3):
    """Create a Bloom filter structure.

    Args:
        size: Number of bits.
        num_hashes: Number of hash functions.

    Returns:
        Bloom filter dict.
    """
    return {
        "bits": [False] * size,
        "size": size,
        "num_hashes": num_hashes,
    }

def _bloom_hash(item, seed, size):
    """Simple hash function for Bloom filter.

    Args:
        item: Item to hash (string).
        seed: Hash seed.
        size: Filter size.

    Returns:
        Hash index.
    """
    h = seed
    for c in str(item).elems():
        h = (h * 31 + ord(c)) % (2 ** 31)
    return h % size

def bloom_add(bf, item):
    """Add item to Bloom filter.

    Args:
        bf: The Bloom filter dict.
        item: Item to add.

    Returns:
        Updated Bloom filter.
    """
    new_bits = list(bf["bits"])
    for i in range(bf["num_hashes"]):
        idx = _bloom_hash(item, i * 17 + 1, bf["size"])
        new_bits[idx] = True
    return {"bits": new_bits, "size": bf["size"], "num_hashes": bf["num_hashes"]}

def bloom_contains(bf, item):
    """Check if item might be in Bloom filter.

    Args:
        bf: The Bloom filter dict.
        item: Item to check.

    Returns:
        True if possibly present, False if definitely not.
    """
    for i in range(bf["num_hashes"]):
        idx = _bloom_hash(item, i * 17 + 1, bf["size"])
        if not bf["bits"][idx]:
            return False
    return True

def bloom_fill_ratio(bf):
    """Get fill ratio of Bloom filter.

    Args:
        bf: The Bloom filter dict.

    Returns:
        Fill ratio (0.0-1.0).
    """
    count = sum([1 for b in bf["bits"] if b])
    return count / bf["size"]

# ============================================================================
# SAFE_LRU: LRU cache operations
# ============================================================================

def create_lru_cache(capacity):
    """Create an LRU cache structure.

    Args:
        capacity: Maximum number of items.

    Returns:
        Result with LRU cache dict or error.
    """
    if capacity <= 0:
        return err("LRU cache capacity must be positive")
    return ok({"items": {}, "order": [], "capacity": capacity})

def lru_get(cache, key):
    """Get item from LRU cache.

    Args:
        cache: The LRU cache dict.
        key: Key to get.

    Returns:
        Result with (value, updated_cache) or error.
    """
    if key not in cache["items"]:
        return err("Key not found: " + str(key))

    # Move to front of order
    new_order = [key] + [k for k in cache["order"] if k != key]
    new_cache = {
        "items": cache["items"],
        "order": new_order,
        "capacity": cache["capacity"],
    }
    return ok((cache["items"][key], new_cache))

def lru_put(cache, key, value):
    """Put item in LRU cache.

    Args:
        cache: The LRU cache dict.
        key: Key to set.
        value: Value to store.

    Returns:
        Updated LRU cache.
    """
    new_items = dict(cache["items"])
    new_order = [k for k in cache["order"] if k != key]

    # Evict if at capacity
    while len(new_order) >= cache["capacity"]:
        evicted = new_order.pop()
        new_items.pop(evicted, None)

    new_items[key] = value
    new_order = [key] + new_order

    return {
        "items": new_items,
        "order": new_order,
        "capacity": cache["capacity"],
    }

def lru_contains(cache, key):
    """Check if key is in LRU cache.

    Args:
        cache: The LRU cache dict.
        key: Key to check.

    Returns:
        True if present, False otherwise.
    """
    return key in cache["items"]

# ============================================================================
# SAFE_GRAPH: Graph operations
# ============================================================================

def create_graph(directed = True):
    """Create a graph structure.

    Args:
        directed: Whether the graph is directed.

    Returns:
        Graph dict.
    """
    return {"nodes": {}, "edges": [], "directed": directed}

def graph_add_node(graph, node_id, data = None):
    """Add node to graph.

    Args:
        graph: The graph dict.
        node_id: Unique node identifier.
        data: Optional node data.

    Returns:
        Result with updated graph or error.
    """
    if node_id in graph["nodes"]:
        return err("Node already exists: " + str(node_id))
    new_nodes = dict(graph["nodes"])
    new_nodes[node_id] = data
    return ok({
        "nodes": new_nodes,
        "edges": graph["edges"],
        "directed": graph["directed"],
    })

def graph_add_edge(graph, from_node, to_node, weight = 1):
    """Add edge to graph.

    Args:
        graph: The graph dict.
        from_node: Source node ID.
        to_node: Target node ID.
        weight: Edge weight.

    Returns:
        Result with updated graph or error.
    """
    if from_node not in graph["nodes"]:
        return err("Source node not found: " + str(from_node))
    if to_node not in graph["nodes"]:
        return err("Target node not found: " + str(to_node))

    edge = {"from": from_node, "to": to_node, "weight": weight}
    new_edges = graph["edges"] + [edge]

    if not graph["directed"]:
        reverse_edge = {"from": to_node, "to": from_node, "weight": weight}
        new_edges = new_edges + [reverse_edge]

    return ok({
        "nodes": graph["nodes"],
        "edges": new_edges,
        "directed": graph["directed"],
    })

def graph_neighbors(graph, node_id):
    """Get neighbors of a node.

    Args:
        graph: The graph dict.
        node_id: Node ID to get neighbors for.

    Returns:
        Result with list of neighbor IDs or error.
    """
    if node_id not in graph["nodes"]:
        return err("Node not found: " + str(node_id))
    neighbors = [e["to"] for e in graph["edges"] if e["from"] == node_id]
    return ok(neighbors)

def graph_has_edge(graph, from_node, to_node):
    """Check if edge exists.

    Args:
        graph: The graph dict.
        from_node: Source node ID.
        to_node: Target node ID.

    Returns:
        True if edge exists, False otherwise.
    """
    for e in graph["edges"]:
        if e["from"] == from_node and e["to"] == to_node:
            return True
    return False

# ============================================================================
# SAFE_RATE_LIMITER: Rate limiting structures
# ============================================================================

def create_token_bucket(capacity, refill_rate):
    """Create a token bucket rate limiter.

    Args:
        capacity: Maximum tokens.
        refill_rate: Tokens added per time unit.

    Returns:
        Token bucket dict.
    """
    return {
        "capacity": capacity,
        "tokens": capacity,
        "refill_rate": refill_rate,
        "last_refill": 0,
    }

def token_bucket_refill(bucket, current_time):
    """Refill token bucket based on elapsed time.

    Args:
        bucket: The token bucket dict.
        current_time: Current timestamp.

    Returns:
        Updated token bucket.
    """
    elapsed = current_time - bucket["last_refill"]
    new_tokens = bucket["refill_rate"] * elapsed
    tokens = min(bucket["capacity"], bucket["tokens"] + new_tokens)
    return {
        "capacity": bucket["capacity"],
        "tokens": tokens,
        "refill_rate": bucket["refill_rate"],
        "last_refill": current_time,
    }

def token_bucket_try_acquire(bucket, count, current_time):
    """Try to acquire tokens from bucket.

    Args:
        bucket: The token bucket dict.
        count: Number of tokens to acquire.
        current_time: Current timestamp.

    Returns:
        Result with (allowed: bool, updated_bucket) or error.
    """
    refilled = token_bucket_refill(bucket, current_time)

    if refilled["tokens"] >= count:
        new_bucket = {
            "capacity": refilled["capacity"],
            "tokens": refilled["tokens"] - count,
            "refill_rate": refilled["refill_rate"],
            "last_refill": refilled["last_refill"],
        }
        return ok((True, new_bucket))

    return ok((False, refilled))

def create_sliding_window(max_requests, window_size):
    """Create a sliding window rate limiter.

    Args:
        max_requests: Maximum requests per window.
        window_size: Window size in time units.

    Returns:
        Sliding window dict.
    """
    return {
        "max_requests": max_requests,
        "window_size": window_size,
        "requests": [],
    }

def sliding_window_try_request(window, current_time):
    """Try to make a request through sliding window.

    Args:
        window: The sliding window dict.
        current_time: Current timestamp.

    Returns:
        Result with (allowed: bool, updated_window).
    """
    cutoff = current_time - window["window_size"]
    active_requests = [ts for ts in window["requests"] if ts >= cutoff]

    if len(active_requests) < window["max_requests"]:
        new_window = {
            "max_requests": window["max_requests"],
            "window_size": window["window_size"],
            "requests": active_requests + [current_time],
        }
        return ok((True, new_window))

    new_window = {
        "max_requests": window["max_requests"],
        "window_size": window["window_size"],
        "requests": active_requests,
    }
    return ok((False, new_window))

# ============================================================================
# SAFE_CIRCUIT_BREAKER: Circuit breaker pattern
# ============================================================================

# Circuit states
CIRCUIT_CLOSED = "closed"
CIRCUIT_OPEN = "open"
CIRCUIT_HALF_OPEN = "half_open"

def create_circuit_breaker(failure_threshold = 5, success_threshold = 2, timeout = 30):
    """Create a circuit breaker.

    Args:
        failure_threshold: Failures before opening.
        success_threshold: Successes in half-open before closing.
        timeout: Time before attempting recovery.

    Returns:
        Circuit breaker dict.
    """
    return {
        "state": CIRCUIT_CLOSED,
        "failures": 0,
        "successes": 0,
        "last_failure_time": 0,
        "failure_threshold": failure_threshold,
        "success_threshold": success_threshold,
        "timeout": timeout,
    }

def circuit_can_execute(cb, current_time):
    """Check if circuit breaker allows execution.

    Args:
        cb: The circuit breaker dict.
        current_time: Current timestamp.

    Returns:
        (can_execute: bool, updated_cb).
    """
    # Check for state transition
    if cb["state"] == CIRCUIT_OPEN:
        if current_time >= cb["last_failure_time"] + cb["timeout"]:
            new_cb = dict(cb)
            new_cb["state"] = CIRCUIT_HALF_OPEN
            new_cb["successes"] = 0
            return (True, new_cb)
        return (False, cb)

    return (True, cb)

def circuit_record_success(cb):
    """Record successful call.

    Args:
        cb: The circuit breaker dict.

    Returns:
        Updated circuit breaker.
    """
    new_cb = dict(cb)

    if cb["state"] == CIRCUIT_CLOSED:
        new_cb["failures"] = 0
    elif cb["state"] == CIRCUIT_HALF_OPEN:
        new_cb["successes"] = cb["successes"] + 1
        if new_cb["successes"] >= cb["success_threshold"]:
            new_cb["state"] = CIRCUIT_CLOSED
            new_cb["failures"] = 0
            new_cb["successes"] = 0

    return new_cb

def circuit_record_failure(cb, current_time):
    """Record failed call.

    Args:
        cb: The circuit breaker dict.
        current_time: Current timestamp.

    Returns:
        Updated circuit breaker.
    """
    new_cb = dict(cb)
    new_cb["last_failure_time"] = current_time
    new_cb["failures"] = cb["failures"] + 1

    if cb["state"] == CIRCUIT_CLOSED:
        if new_cb["failures"] >= cb["failure_threshold"]:
            new_cb["state"] = CIRCUIT_OPEN
    elif cb["state"] == CIRCUIT_HALF_OPEN:
        new_cb["state"] = CIRCUIT_OPEN

    return new_cb

def circuit_is_healthy(cb):
    """Check if circuit is healthy (closed).

    Args:
        cb: The circuit breaker dict.

    Returns:
        True if closed, False otherwise.
    """
    return cb["state"] == CIRCUIT_CLOSED

# ============================================================================
# SAFE_RETRY: Retry strategies
# ============================================================================

def create_retry_config(max_attempts = 3, initial_delay_ms = 100, max_delay_ms = 10000, multiplier = 2.0):
    """Create retry configuration.

    Args:
        max_attempts: Maximum retry attempts.
        initial_delay_ms: Initial delay in milliseconds.
        max_delay_ms: Maximum delay in milliseconds.
        multiplier: Exponential backoff multiplier.

    Returns:
        Retry config dict.
    """
    return {
        "max_attempts": max_attempts,
        "initial_delay_ms": initial_delay_ms,
        "max_delay_ms": max_delay_ms,
        "multiplier": multiplier,
    }

def create_retry_state(config):
    """Create retry state tracker.

    Args:
        config: Retry configuration dict.

    Returns:
        Retry state dict.
    """
    return {
        "config": config,
        "attempts": 0,
        "current_delay_ms": config["initial_delay_ms"],
    }

def retry_should_retry(state):
    """Check if more retries are available.

    Args:
        state: Retry state dict.

    Returns:
        True if retries available, False otherwise.
    """
    return state["attempts"] < state["config"]["max_attempts"]

def retry_record_attempt(state):
    """Record a retry attempt.

    Args:
        state: Retry state dict.

    Returns:
        Result with (delay_ms, updated_state) or None if no retries left.
    """
    if not retry_should_retry(state):
        return err("No retries remaining")

    delay = state["current_delay_ms"]
    next_delay = int(state["current_delay_ms"] * state["config"]["multiplier"])
    next_delay = min(next_delay, state["config"]["max_delay_ms"])

    new_state = {
        "config": state["config"],
        "attempts": state["attempts"] + 1,
        "current_delay_ms": next_delay,
    }

    return ok((delay, new_state))

def exponential_backoff(attempt, base_ms, max_ms, multiplier = 2.0):
    """Calculate exponential backoff delay.

    Args:
        attempt: Attempt number (0-indexed).
        base_ms: Base delay in milliseconds.
        max_ms: Maximum delay in milliseconds.
        multiplier: Backoff multiplier.

    Returns:
        Delay in milliseconds.
    """
    delay = base_ms
    for _ in range(attempt):
        delay = int(delay * multiplier)
    return min(delay, max_ms)

# ============================================================================
# SAFE_MONOTONIC: Monotonic counters and timestamps
# ============================================================================

def create_monotonic_counter(initial = 0):
    """Create a monotonic counter.

    Args:
        initial: Initial value.

    Returns:
        Result with counter dict or error.
    """
    if initial < 0:
        return err("Monotonic counter cannot start negative")
    return ok({"value": initial})

def monotonic_increment(counter, amount = 1):
    """Increment monotonic counter.

    Args:
        counter: The counter dict.
        amount: Amount to increment (must be positive).

    Returns:
        Result with updated counter or error.
    """
    if amount <= 0:
        return err("Increment amount must be positive")
    return ok({"value": counter["value"] + amount})

def monotonic_get(counter):
    """Get current counter value.

    Args:
        counter: The counter dict.

    Returns:
        Current value.
    """
    return counter["value"]

def validate_monotonic_sequence(sequence):
    """Validate that a sequence is monotonically increasing.

    Args:
        sequence: List of values.

    Returns:
        Result with sequence or error indicating where it fails.
    """
    for i in range(1, len(sequence)):
        if sequence[i] <= sequence[i - 1]:
            return err("Sequence not monotonic at index {}: {} <= {}".format(
                i,
                sequence[i],
                sequence[i - 1],
            ))
    return ok(sequence)

# ============================================================================
# SAFE_STATE_MACHINE: State machine definitions
# ============================================================================

def create_state_machine(initial_state, transitions):
    """Create a state machine.

    Args:
        initial_state: Starting state.
        transitions: Dict of state -> list of valid target states.

    Returns:
        State machine dict.
    """
    return {
        "current": initial_state,
        "transitions": transitions,
        "history": [],
    }

def state_machine_can_transition(sm, to_state):
    """Check if transition is valid.

    Args:
        sm: State machine dict.
        to_state: Target state.

    Returns:
        True if valid, False otherwise.
    """
    valid_targets = sm["transitions"].get(sm["current"], [])
    return to_state in valid_targets

def state_machine_transition(sm, to_state):
    """Attempt state transition.

    Args:
        sm: State machine dict.
        to_state: Target state.

    Returns:
        Result with updated state machine or error.
    """
    if not state_machine_can_transition(sm, to_state):
        return err("Invalid transition from '{}' to '{}'".format(sm["current"], to_state))

    new_history = sm["history"] + [sm["current"]]
    # Limit history size
    if len(new_history) > 100:
        new_history = new_history[-100:]

    return ok({
        "current": to_state,
        "transitions": sm["transitions"],
        "history": new_history,
    })

def state_machine_current(sm):
    """Get current state.

    Args:
        sm: State machine dict.

    Returns:
        Current state.
    """
    return sm["current"]

def state_machine_valid_transitions(sm):
    """Get valid transitions from current state.

    Args:
        sm: State machine dict.

    Returns:
        List of valid target states.
    """
    return sm["transitions"].get(sm["current"], [])

# ============================================================================
# SAFE_CALCULATOR: Expression evaluation (limited)
# ============================================================================

def calculate_percentage(value, total):
    """Calculate percentage safely.

    Args:
        value: The part value.
        total: The total value.

    Returns:
        Result with percentage or error.
    """
    if total == 0:
        return err("Cannot calculate percentage with zero total")
    return ok((value * 100.0) / total)

def calculate_ratio(a, b):
    """Calculate ratio safely.

    Args:
        a: Numerator.
        b: Denominator.

    Returns:
        Result with ratio or error.
    """
    if b == 0:
        return err("Cannot calculate ratio with zero denominator")
    return ok(a / b)

def calculate_average(values):
    """Calculate average of values.

    Args:
        values: List of numeric values.

    Returns:
        Result with average or error.
    """
    if not values:
        return err("Cannot calculate average of empty list")
    return ok(sum(values) / len(values))

def calculate_sum(values):
    """Calculate sum of values.

    Args:
        values: List of numeric values.

    Returns:
        Result with sum.
    """
    return ok(sum(values))

def calculate_min(values):
    """Find minimum value.

    Args:
        values: List of numeric values.

    Returns:
        Result with minimum or error.
    """
    if not values:
        return err("Cannot find min of empty list")
    return ok(min(values))

def calculate_max(values):
    """Find maximum value.

    Args:
        values: List of numeric values.

    Returns:
        Result with maximum or error.
    """
    if not values:
        return err("Cannot find max of empty list")
    return ok(max(values))

# ============================================================================
# SAFE_GEO: Geographic calculations
# ============================================================================

# Earth radius constants
EARTH_RADIUS_KM = 6371.0
EARTH_RADIUS_MI = 3958.8

def validate_latitude(lat):
    """Validate latitude.

    Args:
        lat: Latitude in degrees.

    Returns:
        Result with latitude or error.
    """
    if type(lat) not in ["int", "float"]:
        return err("Latitude must be numeric")
    if lat < -90 or lat > 90:
        return err("Latitude must be between -90 and 90")
    return ok(lat)

def validate_longitude(lon):
    """Validate longitude.

    Args:
        lon: Longitude in degrees.

    Returns:
        Result with longitude or error.
    """
    if type(lon) not in ["int", "float"]:
        return err("Longitude must be numeric")
    if lon < -180 or lon > 180:
        return err("Longitude must be between -180 and 180")
    return ok(lon)

def validate_coordinate(lat, lon):
    """Validate geographic coordinate.

    Args:
        lat: Latitude in degrees.
        lon: Longitude in degrees.

    Returns:
        Result with coordinate dict or error.
    """
    lat_result = validate_latitude(lat)
    if is_err(lat_result):
        return lat_result
    lon_result = validate_longitude(lon)
    if is_err(lon_result):
        return lon_result
    return ok({"lat": lat, "lon": lon})

def validate_bounding_box(min_lat, min_lon, max_lat, max_lon):
    """Validate geographic bounding box.

    Args:
        min_lat: Minimum latitude.
        min_lon: Minimum longitude.
        max_lat: Maximum latitude.
        max_lon: Maximum longitude.

    Returns:
        Result with bounding box dict or error.
    """
    if min_lat > max_lat:
        return err("min_lat must be <= max_lat")
    if min_lon > max_lon:
        return err("min_lon must be <= max_lon")

    for name, val in [("min_lat", min_lat), ("max_lat", max_lat)]:
        r = validate_latitude(val)
        if is_err(r):
            return err("{}: {}".format(name, r["error"]))

    for name, val in [("min_lon", min_lon), ("max_lon", max_lon)]:
        r = validate_longitude(val)
        if is_err(r):
            return err("{}: {}".format(name, r["error"]))

    return ok({
        "min_lat": min_lat,
        "min_lon": min_lon,
        "max_lat": max_lat,
        "max_lon": max_lon,
    })

def coordinate_in_bounding_box(coord, bbox):
    """Check if coordinate is within bounding box.

    Args:
        coord: Coordinate dict with lat, lon.
        bbox: Bounding box dict.

    Returns:
        True if within, False otherwise.
    """
    return (coord["lat"] >= bbox["min_lat"] and
            coord["lat"] <= bbox["max_lat"] and
            coord["lon"] >= bbox["min_lon"] and
            coord["lon"] <= bbox["max_lon"])

# ============================================================================
# SAFE_PROBABILITY: Probability and statistics
# ============================================================================

def validate_probability_value(p, field_name = "probability"):
    """Validate probability value (0-1).

    Args:
        p: Probability value.
        field_name: Name for error messages.

    Returns:
        Result with probability or error.
    """
    if type(p) not in ["int", "float"]:
        return err("{} must be numeric".format(field_name))
    if p < 0 or p > 1:
        return err("{} must be between 0 and 1".format(field_name))
    return ok(p)

def validate_probability_distribution(probs):
    """Validate probability distribution sums to 1.

    Args:
        probs: List of probabilities.

    Returns:
        Result with probabilities or error.
    """
    if not probs:
        return err("Probability distribution cannot be empty")

    for i, p in enumerate(probs):
        r = validate_probability_value(p, "probability[{}]".format(i))
        if is_err(r):
            return r

    total = sum(probs)
    if abs(total - 1.0) > 0.0001:
        return err("Probabilities must sum to 1.0, got {}".format(total))

    return ok(probs)

def weighted_choice_validate(weights):
    """Validate weights for weighted random choice.

    Args:
        weights: List of non-negative weights.

    Returns:
        Result with weights or error.
    """
    if not weights:
        return err("Weights cannot be empty")

    for i, w in enumerate(weights):
        if type(w) not in ["int", "float"]:
            return err("Weight[{}] must be numeric".format(i))
        if w < 0:
            return err("Weight[{}] cannot be negative".format(i))

    if sum(weights) == 0:
        return err("At least one weight must be positive")

    return ok(weights)

# ============================================================================
# SAFE_CHECKSUM: Checksum validation
# ============================================================================

def validate_luhn(number_str):
    """Validate Luhn checksum (credit cards, etc.).

    Args:
        number_str: Numeric string to validate.

    Returns:
        Result with True if valid or error.
    """
    if not number_str:
        return err("Number cannot be empty")

    # Remove spaces/dashes
    digits = ""
    for c in number_str.elems():
        if c.isdigit():
            digits += c
        elif c not in " -":
            return err("Invalid character: " + c)

    if not digits:
        return err("No digits found")

    # Luhn algorithm
    total = 0
    for i, c in enumerate(reversed(digits.elems())):
        d = int(c)
        if i % 2 == 1:
            d *= 2
            if d > 9:
                d -= 9
        total += d

    if total % 10 == 0:
        return ok(True)
    return err("Luhn checksum failed")

def calculate_luhn_check_digit(number_str):
    """Calculate Luhn check digit.

    Args:
        number_str: Numeric string without check digit.

    Returns:
        Result with check digit or error.
    """
    if not number_str:
        return err("Number cannot be empty")

    for c in number_str.elems():
        if not c.isdigit():
            return err("Number must contain only digits")

    # Calculate what check digit would make it valid
    for check in range(10):
        if is_ok(validate_luhn(number_str + str(check))):
            return ok(check)

    return err("Could not calculate check digit")

def validate_isbn10(isbn):
    """Validate ISBN-10 checksum.

    Args:
        isbn: ISBN-10 string.

    Returns:
        Result with True if valid or error.
    """
    # Remove hyphens
    digits = isbn.replace("-", "").replace(" ", "")

    if len(digits) != 10:
        return err("ISBN-10 must be 10 characters")

    total = 0
    for i, c in enumerate(digits.elems()):
        if i == 9 and c.upper() == "X":
            total += 10
        elif c.isdigit():
            total += int(c) * (10 - i)
        else:
            return err("Invalid ISBN character: " + c)

    if total % 11 == 0:
        return ok(True)
    return err("ISBN-10 checksum failed")

def validate_isbn13(isbn):
    """Validate ISBN-13 checksum.

    Args:
        isbn: ISBN-13 string.

    Returns:
        Result with True if valid or error.
    """
    digits = isbn.replace("-", "").replace(" ", "")

    if len(digits) != 13:
        return err("ISBN-13 must be 13 digits")

    for c in digits.elems():
        if not c.isdigit():
            return err("ISBN-13 must contain only digits")

    total = 0
    for i, c in enumerate(digits.elems()):
        d = int(c)
        if i % 2 == 0:
            total += d
        else:
            total += d * 3

    if total % 10 == 0:
        return ok(True)
    return err("ISBN-13 checksum failed")

# ============================================================================
# SAFE_TENSOR: Tensor/matrix operations (basic)
# ============================================================================

def validate_tensor_shape(shape):
    """Validate tensor shape.

    Args:
        shape: List of dimension sizes.

    Returns:
        Result with shape or error.
    """
    if not shape:
        return err("Shape cannot be empty")
    for i, dim in enumerate(shape):
        if type(dim) != "int":
            return err("Dimension {} must be an integer".format(i))
        if dim <= 0:
            return err("Dimension {} must be positive".format(i))
    return ok(shape)

def tensor_size(shape):
    """Calculate total tensor size from shape.

    Args:
        shape: List of dimension sizes.

    Returns:
        Result with total element count or error.
    """
    result = validate_tensor_shape(shape)
    if is_err(result):
        return result

    size = 1
    for dim in shape:
        size *= dim
    return ok(size)

def validate_matrix_dimensions(rows, cols):
    """Validate matrix dimensions.

    Args:
        rows: Number of rows.
        cols: Number of columns.

    Returns:
        Result with dimensions dict or error.
    """
    if type(rows) != "int" or type(cols) != "int":
        return err("Dimensions must be integers")
    if rows <= 0 or cols <= 0:
        return err("Dimensions must be positive")
    return ok({"rows": rows, "cols": cols})

def validate_matrix_multiply_dims(a_rows, a_cols, b_rows, b_cols):
    """Validate dimensions for matrix multiplication.

    Args:
        a_rows: Rows in matrix A.
        a_cols: Columns in matrix A.
        b_rows: Rows in matrix B.
        b_cols: Columns in matrix B.

    Returns:
        Result with output dimensions or error.
    """
    if a_cols != b_rows:
        return err("Matrix dimensions incompatible: {}x{} * {}x{}".format(
            a_rows,
            a_cols,
            b_rows,
            b_cols,
        ))
    return ok({"rows": a_rows, "cols": b_cols})

# ============================================================================
# SAFE_PASSWORD: Password validation
# ============================================================================

def validate_password_strength(password, min_length = 8, require_upper = True, require_lower = True, require_digit = True, require_special = False):
    """Validate password strength.

    Args:
        password: The password string.
        min_length: Minimum required length.
        require_upper: Require uppercase letter.
        require_lower: Require lowercase letter.
        require_digit: Require digit.
        require_special: Require special character.

    Returns:
        Result with password or error.
    """
    if len(password) < min_length:
        return err("Password must be at least {} characters".format(min_length))

    has_upper = False
    has_lower = False
    has_digit = False
    has_special = False
    special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"

    for c in password.elems():
        if c.isupper():
            has_upper = True
        elif c.islower():
            has_lower = True
        elif c.isdigit():
            has_digit = True
        elif c in special_chars:
            has_special = True

    if require_upper and not has_upper:
        return err("Password must contain uppercase letter")
    if require_lower and not has_lower:
        return err("Password must contain lowercase letter")
    if require_digit and not has_digit:
        return err("Password must contain digit")
    if require_special and not has_special:
        return err("Password must contain special character")

    return ok(password)

def estimate_password_entropy(password):
    """Estimate password entropy in bits.

    Args:
        password: The password string.

    Returns:
        Result with estimated entropy bits.
    """
    charset_size = 0
    has_lower = False
    has_upper = False
    has_digit = False
    has_special = False

    for c in password.elems():
        if c.islower():
            has_lower = True
        elif c.isupper():
            has_upper = True
        elif c.isdigit():
            has_digit = True
        else:
            has_special = True

    if has_lower:
        charset_size += 26
    if has_upper:
        charset_size += 26
    if has_digit:
        charset_size += 10
    if has_special:
        charset_size += 32

    if charset_size == 0:
        return ok(0)

    # Entropy = length * log2(charset_size)
    # Approximate log2 for common values
    log2_approx = {
        26: 4.7,
        36: 5.17,
        52: 5.7,
        62: 5.95,
        72: 6.17,
        84: 6.39,
        94: 6.55,
    }
    log2_val = log2_approx.get(charset_size, 6.0)

    entropy = int(len(password) * log2_val)
    return ok(entropy)

# ============================================================================
# SAFE_ML: Machine learning validation
# ============================================================================

def validate_feature_vector(features, expected_dim = None):
    """Validate feature vector.

    Args:
        features: List of numeric features.
        expected_dim: Expected dimensionality (optional).

    Returns:
        Result with features or error.
    """
    if not features:
        return err("Feature vector cannot be empty")

    for i, f in enumerate(features):
        if type(f) not in ["int", "float"]:
            return err("Feature[{}] must be numeric".format(i))

    if expected_dim and len(features) != expected_dim:
        return err("Expected {} features, got {}".format(expected_dim, len(features)))

    return ok(features)

def validate_label(label, valid_labels):
    """Validate classification label.

    Args:
        label: The label value.
        valid_labels: List of valid labels.

    Returns:
        Result with label or error.
    """
    if label not in valid_labels:
        return err("Invalid label '{}'. Valid: {}".format(label, valid_labels))
    return ok(label)

def validate_train_test_split(total, train_ratio):
    """Validate train/test split parameters.

    Args:
        total: Total number of samples.
        train_ratio: Ratio for training set (0-1).

    Returns:
        Result with split sizes or error.
    """
    if total <= 0:
        return err("Total samples must be positive")
    if train_ratio <= 0 or train_ratio >= 1:
        return err("Train ratio must be between 0 and 1 (exclusive)")

    train_size = int(total * train_ratio)
    test_size = total - train_size

    if train_size == 0 or test_size == 0:
        return err("Split results in empty set")

    return ok({"train_size": train_size, "test_size": test_size})

def validate_batch_size(batch_size, dataset_size):
    """Validate batch size for training.

    Args:
        batch_size: Proposed batch size.
        dataset_size: Size of dataset.

    Returns:
        Result with batch size or error.
    """
    if batch_size <= 0:
        return err("Batch size must be positive")
    if batch_size > dataset_size:
        return err("Batch size ({}) exceeds dataset size ({})".format(
            batch_size,
            dataset_size,
        ))
    return ok(batch_size)

# ============================================================================
# SAFE_HEADER: HTTP header validation
# ============================================================================

# Common header names
STANDARD_HEADERS = [
    "Accept",
    "Accept-Encoding",
    "Accept-Language",
    "Authorization",
    "Cache-Control",
    "Content-Length",
    "Content-Type",
    "Cookie",
    "Host",
    "If-Modified-Since",
    "If-None-Match",
    "Origin",
    "Referer",
    "User-Agent",
    "X-Forwarded-For",
    "X-Forwarded-Proto",
    "X-Request-Id",
]

def validate_header_name(name):
    """Validate HTTP header name.

    Args:
        name: Header name.

    Returns:
        Result with name or error.
    """
    if not name:
        return err("Header name cannot be empty")

    # Header names are case-insensitive ASCII
    for c in name.elems():
        if not (c.isalnum() or c in "-_"):
            return err("Invalid header name character: " + c)

    return ok(name)

def validate_header_value(value):
    """Validate HTTP header value.

    Args:
        value: Header value.

    Returns:
        Result with value or error.
    """
    # Check for control characters
    for c in value.elems():
        code = ord(c)
        if code < 32 and code not in [9]:  # Allow tab
            return err("Header value contains control character")
    return ok(value)

def validate_authorization_header(auth):
    """Validate Authorization header format.

    Args:
        auth: Authorization header value.

    Returns:
        Result with parsed auth or error.
    """
    if not auth:
        return err("Authorization header cannot be empty")

    parts = auth.split(" ", 1)
    if len(parts) != 2:
        return err("Authorization header must be 'scheme credentials'")

    scheme = parts[0]
    credentials = parts[1]

    valid_schemes = ["Basic", "Bearer", "Digest", "HOBA", "Mutual", "AWS4-HMAC-SHA256"]
    if scheme not in valid_schemes:
        return err("Unknown authorization scheme: " + scheme)

    return ok({"scheme": scheme, "credentials": credentials})

def build_basic_auth(username, password):
    """Build Basic auth header value (base64 encoding not available in Starlark).

    Args:
        username: Username.
        password: Password.

    Returns:
        Result with instruction (base64 must be done externally).
    """
    if not username:
        return err("Username cannot be empty")
    if ":" in username:
        return err("Username cannot contain ':'")
    # Note: Actual base64 encoding not available in Starlark
    return ok("Basic <base64({})>".format(username + ":" + password))

# ============================================================================
# SAFE_COOKIE: HTTP cookie validation
# ============================================================================

def validate_cookie_name(name):
    """Validate cookie name.

    Args:
        name: Cookie name.

    Returns:
        Result with name or error.
    """
    if not name:
        return err("Cookie name cannot be empty")

    # Cookie names cannot contain certain characters
    invalid_chars = "()<>@,;:\\\"/[]?={} \t"
    for c in name.elems():
        if c in invalid_chars or ord(c) < 33 or ord(c) > 126:
            return err("Invalid cookie name character: " + c)

    return ok(name)

def validate_cookie_value(value):
    """Validate cookie value.

    Args:
        value: Cookie value.

    Returns:
        Result with value or error.
    """
    # Cookie values have restricted characters
    for c in value.elems():
        code = ord(c)
        if code < 33 or code > 126 or c in "\",;\\%":
            return err("Invalid cookie value character")

    return ok(value)

def build_set_cookie(name, value, max_age = None, path = None, domain = None, secure = False, http_only = False, same_site = None):
    """Build Set-Cookie header value.

    Args:
        name: Cookie name.
        value: Cookie value.
        max_age: Max age in seconds.
        path: Cookie path.
        domain: Cookie domain.
        secure: Secure flag.
        http_only: HttpOnly flag.
        same_site: SameSite attribute (Strict, Lax, None).

    Returns:
        Result with Set-Cookie header value or error.
    """
    name_result = validate_cookie_name(name)
    if is_err(name_result):
        return name_result

    value_result = validate_cookie_value(value)
    if is_err(value_result):
        return value_result

    cookie = name + "=" + value

    if max_age != None:
        if max_age < 0:
            return err("max_age cannot be negative")
        cookie += "; Max-Age=" + str(max_age)

    if path:
        cookie += "; Path=" + path

    if domain:
        cookie += "; Domain=" + domain

    if secure:
        cookie += "; Secure"

    if http_only:
        cookie += "; HttpOnly"

    if same_site:
        if same_site not in ["Strict", "Lax", "None"]:
            return err("SameSite must be Strict, Lax, or None")
        cookie += "; SameSite=" + same_site

    return ok(cookie)

def parse_cookie_header(cookie_header):
    """Parse Cookie header into dict.

    Args:
        cookie_header: Cookie header value.

    Returns:
        Result with cookie dict or error.
    """
    if not cookie_header:
        return err("Cookie header cannot be empty")

    cookies = {}
    pairs = cookie_header.split(";")

    for pair in pairs:
        pair = pair.strip()
        if not pair:
            continue

        if "=" not in pair:
            return err("Invalid cookie pair: " + pair)

        name, value = pair.split("=", 1)
        name = name.strip()
        value = value.strip()

        name_result = validate_cookie_name(name)
        if is_err(name_result):
            return name_result

        cookies[name] = value

    return ok(cookies)

# ============================================================================
# SAFE_CONTENT_TYPE: Content-Type header handling
# ============================================================================

# Common MIME types
MIME_TYPES = struct(
    json = "application/json",
    xml = "application/xml",
    html = "text/html",
    plain = "text/plain",
    css = "text/css",
    javascript = "application/javascript",
    form_urlencoded = "application/x-www-form-urlencoded",
    multipart_form = "multipart/form-data",
    octet_stream = "application/octet-stream",
    pdf = "application/pdf",
    png = "image/png",
    jpeg = "image/jpeg",
    gif = "image/gif",
    svg = "image/svg+xml",
)

def validate_mime_type(mime_type):
    """Validate MIME type format.

    Args:
        mime_type: MIME type string (e.g., "application/json").

    Returns:
        Result with MIME type or error.
    """
    if not mime_type:
        return err("MIME type cannot be empty")

    if "/" not in mime_type:
        return err("MIME type must contain '/'")

    parts = mime_type.split("/")
    if len(parts) != 2:
        return err("MIME type must have exactly one '/'")

    type_part, subtype_part = parts

    # Handle parameters (e.g., "text/html; charset=utf-8")
    if ";" in subtype_part:
        subtype_part = subtype_part.split(";")[0].strip()

    if not type_part or not subtype_part:
        return err("MIME type and subtype cannot be empty")

    return ok(mime_type)

def parse_content_type(content_type):
    """Parse Content-Type header.

    Args:
        content_type: Content-Type header value.

    Returns:
        Result with parsed components or error.
    """
    result = validate_mime_type(content_type)
    if is_err(result):
        return result

    # Split off parameters
    parts = content_type.split(";")
    mime = parts[0].strip()

    params = {}
    for param in parts[1:]:
        param = param.strip()
        if "=" in param:
            key, value = param.split("=", 1)
            params[key.strip()] = value.strip().strip('"')

    return ok({"mime_type": mime, "parameters": params})

def get_charset(content_type, default = "utf-8"):
    """Extract charset from Content-Type.

    Args:
        content_type: Content-Type header value.
        default: Default charset if not specified.

    Returns:
        Result with charset string.
    """
    result = parse_content_type(content_type)
    if is_err(result):
        return ok(default)

    parsed = unwrap(result)
    return ok(parsed["parameters"].get("charset", default))

def build_content_type(mime_type, charset = None, boundary = None):
    """Build Content-Type header value.

    Args:
        mime_type: Base MIME type.
        charset: Character set (optional).
        boundary: Multipart boundary (optional).

    Returns:
        Result with Content-Type value or error.
    """
    result = validate_mime_type(mime_type)
    if is_err(result):
        return result

    ct = mime_type

    if charset:
        ct += "; charset=" + charset

    if boundary:
        if not mime_type.startswith("multipart/"):
            return err("Boundary only valid for multipart types")
        ct += "; boundary=" + boundary

    return ok(ct)

# ============================================================================
# VALIDATION COMBINATORS
# ============================================================================

def validate_positive(n, field_name = "number"):
    """Validate number is positive.

    Args:
        n: The number to validate.
        field_name: Name of the field for error messages.

    Returns:
        Result with the number or error.
    """
    if type(n) != "int":
        return err("Field '{}' must be an integer".format(field_name))
    if n <= 0:
        return err("Field '{}' must be positive, got: {}".format(field_name, n))
    return ok(n)

def validate_range(n, min_val, max_val, field_name = "number"):
    """Validate number is within range [min, max].

    Args:
        n: The number to validate.
        min_val: Minimum allowed value.
        max_val: Maximum allowed value.
        field_name: Name of the field for error messages.

    Returns:
        Result with the number or error.
    """
    if type(n) != "int":
        return err("Field '{}' must be an integer".format(field_name))
    if n < min_val or n > max_val:
        return err("Field '{}' must be between {} and {}, got: {}".format(
            field_name,
            min_val,
            max_val,
            n,
        ))
    return ok(n)

def validate_one_of(value, allowed, field_name = "value"):
    """Validate value is one of the allowed values.

    Args:
        value: The value to validate.
        allowed: List of allowed values.
        field_name: Name of the field for error messages.

    Returns:
        Result with the value or error.
    """
    if value not in allowed:
        return err("Field '{}' must be one of {}, got: {}".format(
            field_name,
            allowed,
            value,
        ))
    return ok(value)

def validate_all(results):
    """All validations must pass.

    Args:
        results: List of Result values.

    Returns:
        Result with list of values if all Ok, or first error.
    """
    values = []
    for r in results:
        if is_err(r):
            return r
        values.append(r["value"])
    return ok(values)

def validate_any(results):
    """At least one validation must pass.

    Args:
        results: List of Result values.

    Returns:
        First Ok result, or error if all failed.
    """
    for r in results:
        if is_ok(r):
            return r
    return err("All validations failed")

def validate_struct(obj, validators):
    """Validate struct fields.

    Args:
        obj: Dict or struct to validate.
        validators: Dict of field_name -> validator_fn.

    Returns:
        Result with validated object.
    """
    for field_name, validator in validators.items():
        value = getattr(obj, field_name, None) if hasattr(obj, field_name) else obj.get(field_name)
        if value == None:
            return err("Missing required field: " + field_name)
        result = validator(value)
        if is_err(result):
            return result
    return ok(obj)

# ============================================================================
# RESOURCE HELPERS
# ============================================================================

def validate_replicas(count, min_replicas = 1, max_replicas = 100):
    """Validate replica count.

    Args:
        count: Number of replicas.
        min_replicas: Minimum allowed.
        max_replicas: Maximum allowed.

    Returns:
        Result with count or error.
    """
    return validate_range(count, min_replicas, max_replicas, "replicas")

# Common timeouts
SAFE_TIMEOUT_SECONDS = struct(
    short = 30,
    medium = 300,
    long = 1800,
    very_long = 3600,
)

# ============================================================================
# BAZEL RULE HELPERS
# ============================================================================

def proven_library(name, modules = None, visibility = None):
    """Create a proven library target configuration.

    Args:
        name: Target name.
        modules: List of modules to include (default: all).
        visibility: Visibility specification.

    Returns:
        Configuration dict for library target.
    """
    all_modules = []
    for category_modules in MODULE_CATEGORIES.values():
        all_modules.extend(category_modules)

    selected = modules if modules else all_modules

    for m in selected:
        if m not in all_modules:
            fail("Unknown module: {}. Available: {}".format(m, all_modules))

    return {
        "name": name,
        "modules": selected,
        "visibility": visibility or ["//visibility:private"],
        "version": VERSION,
    }

def proven_test(name, target, test_modules = None):
    """Create a proven test target configuration.

    Args:
        name: Test target name.
        target: Target to test.
        test_modules: Specific modules to test.

    Returns:
        Configuration dict for test target.
    """
    return {
        "name": name,
        "target": target,
        "test_modules": test_modules,
    }

# ============================================================================
# MODULE INFO
# ============================================================================

def get_module_info():
    """Get information about all modules.

    Returns:
        Dict with module categories and counts.
    """
    return {
        "version": VERSION,
        "module_count": MODULE_COUNT,
        "categories": MODULE_CATEGORIES,
    }

def list_modules(category = None):
    """List available modules.

    Args:
        category: Optional category filter.

    Returns:
        List of module names.
    """
    if category:
        return MODULE_CATEGORIES.get(category, [])

    all_modules = []
    for modules in MODULE_CATEGORIES.values():
        all_modules.extend(modules)
    return all_modules
