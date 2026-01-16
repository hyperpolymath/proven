# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""Proven Safety Primitives for Starlark/Bazel.

Provides type-safe value constructors and validation functions
for use in Bazel BUILD files and .bzl extensions.
"""

# Version
PROVEN_VERSION = "0.9.0"

# ============================================================================
# RESULT TYPE
# ============================================================================

def ok(value):
    """Create a successful Result."""
    return {"ok": True, "value": value}

def err(error):
    """Create an error Result."""
    return {"ok": False, "error": error}

def is_ok(result):
    """Check if Result is Ok."""
    return result.get("ok", False)

def is_err(result):
    """Check if Result is Err."""
    return not result.get("ok", True)

def unwrap(result):
    """Unwrap Result value, failing if Err."""
    if is_ok(result):
        return result["value"]
    fail("Unwrap on Err: " + str(result.get("error", "unknown")))

def unwrap_or(result, default):
    """Unwrap Result value or return default."""
    return result["value"] if is_ok(result) else default

def map_result(result, fn):
    """Transform Result value if Ok."""
    if is_ok(result):
        return ok(fn(result["value"]))
    return result

# ============================================================================
# SAFE VALIDATION FUNCTIONS
# ============================================================================

def validate_non_empty(s, field_name = "string"):
    """Validate string is non-empty."""
    if not s:
        return err("Field '{}' cannot be empty".format(field_name))
    return ok(s)

def validate_port(port):
    """Validate port number is in valid range (1-65535)."""
    if type(port) != "int":
        return err("Port must be an integer, got: " + type(port))
    if port < 1 or port > 65535:
        return err("Port must be between 1 and 65535, got: " + str(port))
    return ok(port)

def validate_positive(n, field_name = "number"):
    """Validate number is positive."""
    if type(n) != "int":
        return err("Field '{}' must be an integer".format(field_name))
    if n <= 0:
        return err("Field '{}' must be positive, got: {}".format(field_name, n))
    return ok(n)

def validate_range(n, min_val, max_val, field_name = "number"):
    """Validate number is within range [min, max]."""
    if type(n) != "int":
        return err("Field '{}' must be an integer".format(field_name))
    if n < min_val or n > max_val:
        return err("Field '{}' must be between {} and {}, got: {}".format(
            field_name, min_val, max_val, n))
    return ok(n)

def validate_one_of(value, allowed, field_name = "value"):
    """Validate value is one of the allowed values."""
    if value not in allowed:
        return err("Field '{}' must be one of {}, got: {}".format(
            field_name, allowed, value))
    return ok(value)

# ============================================================================
# SAFE MATH
# ============================================================================

# Integer bounds for 64-bit signed integers
_INT64_MAX = 9223372036854775807
_INT64_MIN = -9223372036854775808

def safe_add(a, b):
    """Safe addition with overflow checking."""
    result = a + b
    if result > _INT64_MAX or result < _INT64_MIN:
        return err("Integer overflow in addition")
    return ok(result)

def safe_sub(a, b):
    """Safe subtraction with overflow checking."""
    result = a - b
    if result > _INT64_MAX or result < _INT64_MIN:
        return err("Integer overflow in subtraction")
    return ok(result)

def safe_mul(a, b):
    """Safe multiplication with overflow checking."""
    if a == 0 or b == 0:
        return ok(0)
    result = a * b
    # Check for overflow by division
    if result // a != b:
        return err("Integer overflow in multiplication")
    return ok(result)

def safe_div(a, b):
    """Safe division (no divide by zero)."""
    if b == 0:
        return err("Division by zero")
    return ok(a // b)

def safe_mod(a, b):
    """Safe modulo (no divide by zero)."""
    if b == 0:
        return err("Division by zero")
    return ok(a % b)

def clamp(value, min_val, max_val):
    """Clamp value to range [min, max]."""
    return max(min_val, min(max_val, value))

# ============================================================================
# RESOURCE HELPERS
# ============================================================================

def memory_bytes(value, unit):
    """Convert memory to bytes.

    Args:
        value: Numeric value
        unit: One of "B", "KB", "MB", "GB", "TB"

    Returns:
        Result with bytes value
    """
    units = {
        "B": 1,
        "KB": 1024,
        "MB": 1024 * 1024,
        "GB": 1024 * 1024 * 1024,
        "TB": 1024 * 1024 * 1024 * 1024,
    }
    if unit not in units:
        return err("Unknown memory unit: " + unit)
    if value <= 0:
        return err("Memory must be positive")
    return ok(value * units[unit])

def cpu_millicores(value, unit = "cores"):
    """Convert CPU to millicores.

    Args:
        value: Numeric value
        unit: One of "m", "millicores", "cores", "" (default: cores)

    Returns:
        Result with millicores value
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

def validate_replicas(count, min_replicas = 1, max_replicas = 100):
    """Validate replica count."""
    return validate_range(count, min_replicas, max_replicas, "replicas")

# ============================================================================
# COMMON SAFE VALUES
# ============================================================================

COMMON_PORTS = struct(
    http = 80,
    https = 443,
    ssh = 22,
    dns = 53,
    mysql = 3306,
    postgres = 5432,
    redis = 6379,
    grpc = 50051,
)

SAFE_TIMEOUT_SECONDS = struct(
    short = 30,
    medium = 300,
    long = 1800,
    very_long = 3600,
)

# ============================================================================
# VALIDATION COMBINATORS
# ============================================================================

def validate_all(results):
    """All validations must pass.

    Args:
        results: List of Result values

    Returns:
        Result with list of values if all Ok, or first error
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
        results: List of Result values

    Returns:
        First Ok result, or error if all failed
    """
    for r in results:
        if is_ok(r):
            return r
    return err("All validations failed")

def validate_struct(obj, validators):
    """Validate struct fields.

    Args:
        obj: Dict or struct to validate
        validators: Dict of field_name -> validator_fn

    Returns:
        Result with validated object
    """
    for field_name, validator in validators.items():
        value = getattr(obj, field_name, None) if hasattr(obj, field_name) else obj.get(field_name)
        if value == None:
            return err("Missing required field: " + field_name)
        result = validator(value)
        if is_err(result):
            return result
    return ok(obj)
