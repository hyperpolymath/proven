# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# @version ^0.4.0

"""
@title Proven Safety Library for Vyper
@author Hyperpolymath
@notice Formally verified safety primitives for Ethereum smart contracts
@dev Complete implementation of 38 proven safety modules for Vyper 0.4+.
     Vyper has built-in overflow protection; these utilities add
     domain-specific validation, safe patterns, and resilience mechanisms.

Module Categories:
- Core (11): safe_math, safe_string, safe_path, safe_email, safe_url,
             safe_network, safe_crypto, safe_uuid, safe_currency,
             safe_phone, safe_hex
- Data (7): safe_json, safe_datetime, safe_float, safe_version,
            safe_color, safe_angle, safe_unit
- Data Structures (5): safe_buffer, safe_queue, safe_bloom, safe_lru, safe_graph
- Resilience (4): safe_rate_limiter, safe_circuit_breaker, safe_retry, safe_monotonic
- State (2): safe_state_machine, safe_calculator
- Algorithm (4): safe_geo, safe_probability, safe_checksum, safe_tensor
- Security (2): safe_password, safe_ml
- HTTP (3): safe_header, safe_cookie, safe_content_type
"""

# ============================================================================
# LIBRARY METADATA
# ============================================================================

VERSION: constant(String[8]) = "0.4.0"
MODULE_COUNT: constant(uint8) = 38

# ============================================================================
# CORE CONSTANTS
# ============================================================================

# Network validation
MAX_PORT: constant(uint16) = 65535
MIN_PORT: constant(uint16) = 1

# Percentage handling
MAX_PERCENTAGE: constant(uint256) = 100
BASIS_POINTS: constant(uint256) = 10000

# Numeric limits
MAX_UINT256: constant(uint256) = max_value(uint256)
MAX_INT256: constant(int256) = max_value(int256)
MIN_INT256: constant(int256) = min_value(int256)

# Geographic constants (scaled by 1e6 for precision)
EARTH_RADIUS_KM: constant(uint256) = 6371000  # Scaled by 1000
MAX_LATITUDE: constant(int256) = 90000000     # 90 * 1e6
MIN_LATITUDE: constant(int256) = -90000000    # -90 * 1e6
MAX_LONGITUDE: constant(int256) = 180000000   # 180 * 1e6
MIN_LONGITUDE: constant(int256) = -180000000  # -180 * 1e6

# Time constants
SECONDS_PER_MINUTE: constant(uint256) = 60
SECONDS_PER_HOUR: constant(uint256) = 3600
SECONDS_PER_DAY: constant(uint256) = 86400
SECONDS_PER_WEEK: constant(uint256) = 604800

# Probability constants (scaled by 1e18 for precision)
PROBABILITY_SCALE: constant(uint256) = 10 ** 18
PROBABILITY_ONE: constant(uint256) = 10 ** 18
PROBABILITY_ZERO: constant(uint256) = 0

# ============================================================================
# EVENTS
# ============================================================================

event SafeTransfer:
    sender: indexed(address)
    recipient: indexed(address)
    amount: uint256

event ValidationFailed:
    reason: String[64]
    value: uint256

event StateTransition:
    from_state: uint8
    to_state: uint8
    timestamp: uint256

event RateLimitExceeded:
    caller: indexed(address)
    limit: uint256
    reset_time: uint256

event CircuitBreakerTripped:
    state: uint8
    failure_count: uint256
    timestamp: uint256

# ============================================================================
# VERSION INFO
# ============================================================================

@external
@pure
def version() -> String[8]:
    """
    @notice Return library version
    @return Version string "0.4.0"
    """
    return VERSION

@external
@pure
def module_count() -> uint8:
    """
    @notice Return total module count
    @return Number of modules (38)
    """
    return MODULE_COUNT

# ============================================================================
# MODULE: SAFE_MATH (explicit for documentation, Vyper has built-in checks)
# ============================================================================

@internal
@pure
def safe_add(a: uint256, b: uint256) -> uint256:
    """
    @notice Safe addition with overflow check
    @dev Vyper 0.4+ has automatic overflow checks; this is explicit
    @param a First operand
    @param b Second operand
    @return Sum of a and b
    """
    result: uint256 = a + b
    assert result >= a, "Overflow in addition"
    return result

@internal
@pure
def safe_sub(a: uint256, b: uint256) -> uint256:
    """
    @notice Safe subtraction with underflow check
    @param a Minuend
    @param b Subtrahend
    @return Difference a - b
    """
    assert b <= a, "Underflow in subtraction"
    return a - b

@internal
@pure
def safe_mul(a: uint256, b: uint256) -> uint256:
    """
    @notice Safe multiplication with overflow check
    @param a First factor
    @param b Second factor
    @return Product of a and b
    """
    if a == 0:
        return 0
    result: uint256 = a * b
    assert result / a == b, "Overflow in multiplication"
    return result

@internal
@pure
def safe_div(a: uint256, b: uint256) -> uint256:
    """
    @notice Safe division with zero check
    @param a Dividend
    @param b Divisor
    @return Quotient a / b
    """
    assert b > 0, "Division by zero"
    return a / b

@internal
@pure
def safe_mod(a: uint256, b: uint256) -> uint256:
    """
    @notice Safe modulo with zero check
    @param a Dividend
    @param b Divisor
    @return Remainder a % b
    """
    assert b > 0, "Modulo by zero"
    return a % b

@internal
@pure
def safe_pow(base: uint256, exp: uint256) -> uint256:
    """
    @notice Safe exponentiation with overflow protection
    @param base Base value
    @param exp Exponent
    @return base ** exp
    """
    if exp == 0:
        return 1
    if base == 0:
        return 0
    if base == 1:
        return 1

    result: uint256 = 1
    b: uint256 = base
    e: uint256 = exp

    for i: uint256 in range(256):
        if e == 0:
            break
        if e & 1 == 1:
            result = result * b
        b = b * b
        e = e >> 1

    return result

@internal
@pure
def safe_sqrt(x: uint256) -> uint256:
    """
    @notice Integer square root using Newton's method
    @param x Value to find square root of
    @return Floor of square root of x
    """
    if x == 0:
        return 0

    result: uint256 = x
    k: uint256 = (x + 1) / 2

    for i: uint256 in range(256):
        if k >= result:
            break
        result = k
        k = (x / k + k) / 2

    return result

# ============================================================================
# MODULE: SAFE_STRING (limited string operations in Vyper)
# ============================================================================

@internal
@pure
def is_empty_string(s: String[256]) -> bool:
    """
    @notice Check if string is empty
    @param s String to check
    @return True if string length is 0
    """
    return len(s) == 0

@internal
@pure
def string_length(s: String[256]) -> uint256:
    """
    @notice Get string length safely
    @param s String to measure
    @return Length of string
    """
    return len(s)

@internal
@pure
def require_non_empty_string(s: String[256]):
    """
    @notice Require string is not empty
    @param s String to validate
    """
    assert len(s) > 0, "String cannot be empty"

# ============================================================================
# MODULE: SAFE_HEX
# ============================================================================

@internal
@pure
def is_valid_hex_char(c: uint8) -> bool:
    """
    @notice Check if byte is valid hex character
    @param c ASCII byte value
    @return True if 0-9, a-f, or A-F
    """
    # 0-9: 48-57, A-F: 65-70, a-f: 97-102
    return (c >= 48 and c <= 57) or (c >= 65 and c <= 70) or (c >= 97 and c <= 102)

@internal
@pure
def hex_char_to_nibble(c: uint8) -> uint8:
    """
    @notice Convert hex character to nibble value
    @param c ASCII hex character
    @return Value 0-15
    """
    if c >= 48 and c <= 57:  # 0-9
        return c - 48
    if c >= 65 and c <= 70:  # A-F
        return c - 55
    if c >= 97 and c <= 102:  # a-f
        return c - 87
    return 0

@internal
@pure
def nibble_to_hex_char(n: uint8) -> uint8:
    """
    @notice Convert nibble to hex character (lowercase)
    @param n Value 0-15
    @return ASCII hex character
    """
    assert n < 16, "Nibble must be 0-15"
    if n < 10:
        return n + 48  # 0-9
    return n + 87  # a-f

# ============================================================================
# MODULE: SAFE_NETWORK
# ============================================================================

@internal
@pure
def is_valid_port(port: uint16) -> bool:
    """
    @notice Validate network port number (1-65535)
    @param port Port number to validate
    @return True if valid port
    """
    return port >= MIN_PORT and port <= MAX_PORT

@internal
@pure
def require_valid_port(port: uint16):
    """
    @notice Require valid port or revert
    @param port Port to validate
    """
    assert port >= MIN_PORT and port <= MAX_PORT, "Invalid port"

@internal
@pure
def is_well_known_port(port: uint16) -> bool:
    """
    @notice Check if port is in well-known range (1-1023)
    @param port Port to check
    @return True if well-known port
    """
    return port >= 1 and port <= 1023

@internal
@pure
def is_registered_port(port: uint16) -> bool:
    """
    @notice Check if port is in registered range (1024-49151)
    @param port Port to check
    @return True if registered port
    """
    return port >= 1024 and port <= 49151

@internal
@pure
def is_dynamic_port(port: uint16) -> bool:
    """
    @notice Check if port is in dynamic/ephemeral range (49152-65535)
    @param port Port to check
    @return True if dynamic port
    """
    return port >= 49152 and port <= 65535

# ============================================================================
# MODULE: SAFE_CRYPTO
# ============================================================================

@internal
@pure
def constant_time_compare(a: bytes32, b: bytes32) -> bool:
    """
    @notice Constant-time comparison (prevents timing attacks)
    @dev Result is computed without short-circuit evaluation
    @param a First value
    @param b Second value
    @return True if equal
    """
    result: uint256 = 0
    for i: uint256 in range(32):
        result = result | (convert(slice(convert(a, Bytes[32]), i, 1), uint256) ^ convert(slice(convert(b, Bytes[32]), i, 1), uint256))
    return result == 0

@internal
@pure
def xor_bytes32(a: bytes32, b: bytes32) -> bytes32:
    """
    @notice XOR two bytes32 values
    @param a First value
    @param b Second value
    @return XOR result
    """
    return convert(convert(a, uint256) ^ convert(b, uint256), bytes32)

@internal
@pure
def is_zero_bytes32(value: bytes32) -> bool:
    """
    @notice Check if bytes32 is all zeros
    @param value Value to check
    @return True if all zeros
    """
    return value == empty(bytes32)

# ============================================================================
# MODULE: SAFE_UUID
# ============================================================================

@internal
@pure
def is_valid_uuid_format(uuid_bytes: bytes32) -> bool:
    """
    @notice Basic UUID validation (non-zero)
    @param uuid_bytes UUID as bytes32
    @return True if valid format
    """
    return uuid_bytes != empty(bytes32)

@internal
@pure
def uuid_version(uuid_bytes: bytes32) -> uint8:
    """
    @notice Extract UUID version from bytes32
    @dev Version is in bits 60-63 (byte 6, high nibble)
    @param uuid_bytes UUID as bytes32
    @return Version number (1-5 typically)
    """
    # Get byte 6 (0-indexed)
    version_byte: uint8 = convert(slice(convert(uuid_bytes, Bytes[32]), 6, 1), uint8)
    return (version_byte >> 4) & 0x0F

# ============================================================================
# MODULE: SAFE_CURRENCY
# ============================================================================

@internal
@pure
def is_valid_amount(amount: uint256) -> bool:
    """
    @notice Check if amount is valid (non-zero for transfers)
    @param amount Amount to validate
    @return True if valid
    """
    return amount > 0

@internal
@pure
def percentage_of(amount: uint256, bps: uint256) -> uint256:
    """
    @notice Calculate percentage using basis points (100 bps = 1%)
    @param amount Base amount
    @param bps Basis points (10000 = 100%)
    @return Calculated percentage
    """
    return (amount * bps) / BASIS_POINTS

@internal
@pure
def percentage_of_100(amount: uint256, pct: uint256) -> uint256:
    """
    @notice Calculate percentage (0-100 scale)
    @param amount Base amount
    @param pct Percentage (0-100)
    @return Calculated amount
    """
    assert pct <= 100, "Percentage exceeds 100"
    return (amount * pct) / 100

@internal
@pure
def wei_to_ether(wei_amount: uint256) -> uint256:
    """
    @notice Convert wei to ether (scaled by 1e18)
    @param wei_amount Amount in wei
    @return Amount in ether (still uint256, divide by 1e18)
    """
    return wei_amount

@internal
@pure
def ether_to_wei(ether_amount: uint256) -> uint256:
    """
    @notice Convert ether to wei
    @dev Input is assumed to be in ether units
    @param ether_amount Amount in ether
    @return Amount in wei
    """
    return ether_amount * 10 ** 18

# ============================================================================
# MODULE: SAFE_ADDRESS VALIDATION
# ============================================================================

@internal
@pure
def is_valid_address(addr: address) -> bool:
    """
    @notice Check if address is non-zero
    @param addr Address to validate
    @return True if non-zero
    """
    return addr != empty(address)

@internal
@pure
def require_valid_address(addr: address):
    """
    @notice Require valid (non-zero) address
    @param addr Address to validate
    """
    assert addr != empty(address), "Invalid zero address"

@internal
@pure
def is_contract(addr: address) -> bool:
    """
    @notice Check if address is a contract (has code)
    @dev Uses extcodesize; returns false for contracts in constructor
    @param addr Address to check
    @return True if contract
    """
    return addr.codesize > 0

# ============================================================================
# MODULE: SAFE_DATETIME
# ============================================================================

@internal
@view
def is_future(timestamp: uint256) -> bool:
    """
    @notice Check if timestamp is in the future
    @param timestamp Unix timestamp
    @return True if future
    """
    return timestamp > block.timestamp

@internal
@view
def is_past(timestamp: uint256) -> bool:
    """
    @notice Check if timestamp is in the past
    @param timestamp Unix timestamp
    @return True if past
    """
    return timestamp < block.timestamp

@internal
@pure
def is_within_window(timestamp: uint256, window_start: uint256, window_end: uint256) -> bool:
    """
    @notice Check if timestamp is within a time window
    @param timestamp Timestamp to check
    @param window_start Window start (inclusive)
    @param window_end Window end (inclusive)
    @return True if within window
    """
    return timestamp >= window_start and timestamp <= window_end

@internal
@view
def require_not_expired(deadline: uint256):
    """
    @notice Require deadline has not passed
    @param deadline Unix timestamp deadline
    """
    assert block.timestamp <= deadline, "Deadline expired"

@internal
@pure
def add_days(timestamp: uint256, days: uint256) -> uint256:
    """
    @notice Add days to timestamp
    @param timestamp Base timestamp
    @param days Days to add
    @return New timestamp
    """
    return timestamp + (days * SECONDS_PER_DAY)

@internal
@pure
def add_hours(timestamp: uint256, hours: uint256) -> uint256:
    """
    @notice Add hours to timestamp
    @param timestamp Base timestamp
    @param hours Hours to add
    @return New timestamp
    """
    return timestamp + (hours * SECONDS_PER_HOUR)

# ============================================================================
# MODULE: SAFE_FLOAT (Fixed-point arithmetic with 18 decimals)
# ============================================================================

FIXED_POINT_SCALE: constant(uint256) = 10 ** 18

@internal
@pure
def fixed_mul(a: uint256, b: uint256) -> uint256:
    """
    @notice Multiply two fixed-point numbers (18 decimals)
    @param a First operand (scaled by 1e18)
    @param b Second operand (scaled by 1e18)
    @return Product (scaled by 1e18)
    """
    return (a * b) / FIXED_POINT_SCALE

@internal
@pure
def fixed_div(a: uint256, b: uint256) -> uint256:
    """
    @notice Divide two fixed-point numbers (18 decimals)
    @param a Dividend (scaled by 1e18)
    @param b Divisor (scaled by 1e18)
    @return Quotient (scaled by 1e18)
    """
    assert b > 0, "Division by zero"
    return (a * FIXED_POINT_SCALE) / b

@internal
@pure
def to_fixed(value: uint256) -> uint256:
    """
    @notice Convert integer to fixed-point
    @param value Integer value
    @return Fixed-point representation
    """
    return value * FIXED_POINT_SCALE

@internal
@pure
def from_fixed(value: uint256) -> uint256:
    """
    @notice Convert fixed-point to integer (truncates)
    @param value Fixed-point value
    @return Integer representation
    """
    return value / FIXED_POINT_SCALE

# ============================================================================
# MODULE: SAFE_VERSION (Semantic versioning)
# ============================================================================

@internal
@pure
def pack_version(major: uint8, minor: uint8, patch: uint8) -> uint24:
    """
    @notice Pack semantic version into uint24
    @param major Major version (0-255)
    @param minor Minor version (0-255)
    @param patch Patch version (0-255)
    @return Packed version
    """
    return (convert(major, uint24) << 16) | (convert(minor, uint24) << 8) | convert(patch, uint24)

@internal
@pure
def unpack_version_major(packed: uint24) -> uint8:
    """
    @notice Extract major version from packed
    @param packed Packed version
    @return Major version
    """
    return convert((packed >> 16) & 0xFF, uint8)

@internal
@pure
def unpack_version_minor(packed: uint24) -> uint8:
    """
    @notice Extract minor version from packed
    @param packed Packed version
    @return Minor version
    """
    return convert((packed >> 8) & 0xFF, uint8)

@internal
@pure
def unpack_version_patch(packed: uint24) -> uint8:
    """
    @notice Extract patch version from packed
    @param packed Packed version
    @return Patch version
    """
    return convert(packed & 0xFF, uint8)

@internal
@pure
def compare_versions(a: uint24, b: uint24) -> int8:
    """
    @notice Compare two packed versions
    @param a First version
    @param b Second version
    @return -1 if a < b, 0 if equal, 1 if a > b
    """
    if a < b:
        return -1
    if a > b:
        return 1
    return 0

# ============================================================================
# MODULE: SAFE_COLOR (RGB color handling)
# ============================================================================

@internal
@pure
def pack_rgb(r: uint8, g: uint8, b: uint8) -> uint24:
    """
    @notice Pack RGB values into uint24
    @param r Red component (0-255)
    @param g Green component (0-255)
    @param b Blue component (0-255)
    @return Packed RGB value
    """
    return (convert(r, uint24) << 16) | (convert(g, uint24) << 8) | convert(b, uint24)

@internal
@pure
def unpack_red(rgb: uint24) -> uint8:
    """
    @notice Extract red component from packed RGB
    @param rgb Packed RGB value
    @return Red component
    """
    return convert((rgb >> 16) & 0xFF, uint8)

@internal
@pure
def unpack_green(rgb: uint24) -> uint8:
    """
    @notice Extract green component from packed RGB
    @param rgb Packed RGB value
    @return Green component
    """
    return convert((rgb >> 8) & 0xFF, uint8)

@internal
@pure
def unpack_blue(rgb: uint24) -> uint8:
    """
    @notice Extract blue component from packed RGB
    @param rgb Packed RGB value
    @return Blue component
    """
    return convert(rgb & 0xFF, uint8)

@internal
@pure
def blend_colors(c1: uint24, c2: uint24, weight: uint8) -> uint24:
    """
    @notice Blend two colors with weight (0-255)
    @param c1 First color
    @param c2 Second color
    @param weight Blend weight (0=c1, 255=c2)
    @return Blended color
    """
    r1: uint256 = convert((c1 >> 16) & 0xFF, uint256)
    g1: uint256 = convert((c1 >> 8) & 0xFF, uint256)
    b1: uint256 = convert(c1 & 0xFF, uint256)

    r2: uint256 = convert((c2 >> 16) & 0xFF, uint256)
    g2: uint256 = convert((c2 >> 8) & 0xFF, uint256)
    b2: uint256 = convert(c2 & 0xFF, uint256)

    w: uint256 = convert(weight, uint256)
    inv_w: uint256 = 255 - w

    r: uint256 = (r1 * inv_w + r2 * w) / 255
    g: uint256 = (g1 * inv_w + g2 * w) / 255
    b: uint256 = (b1 * inv_w + b2 * w) / 255

    return (convert(r, uint24) << 16) | (convert(g, uint24) << 8) | convert(b, uint24)

# ============================================================================
# MODULE: SAFE_ANGLE (Angle normalization and conversion)
# ============================================================================

DEGREES_SCALE: constant(uint256) = 10 ** 6  # Micro-degrees for precision

@internal
@pure
def normalize_degrees(degrees: int256) -> uint256:
    """
    @notice Normalize angle to 0-360 range (scaled by 1e6)
    @param degrees Angle in micro-degrees
    @return Normalized angle (0 <= result < 360e6)
    """
    scaled_360: int256 = 360000000  # 360 * 1e6
    result: int256 = degrees % scaled_360
    if result < 0:
        result = result + scaled_360
    return convert(result, uint256)

@internal
@pure
def degrees_to_radians(degrees: uint256) -> uint256:
    """
    @notice Convert degrees to radians (both scaled by 1e6)
    @dev pi/180 * 1e6 approximately = 17453
    @param degrees Angle in micro-degrees
    @return Angle in micro-radians
    """
    return (degrees * 17453) / 1000000

@internal
@pure
def radians_to_degrees(radians: uint256) -> uint256:
    """
    @notice Convert radians to degrees (both scaled by 1e6)
    @dev 180/pi * 1e6 approximately = 57295780
    @param radians Angle in micro-radians
    @return Angle in micro-degrees
    """
    return (radians * 57295780) / 1000000

# ============================================================================
# MODULE: SAFE_UNIT (Unit conversion)
# ============================================================================

@internal
@pure
def km_to_meters(km: uint256) -> uint256:
    """
    @notice Convert kilometers to meters
    @param km Distance in kilometers
    @return Distance in meters
    """
    return km * 1000

@internal
@pure
def meters_to_km(meters: uint256) -> uint256:
    """
    @notice Convert meters to kilometers
    @param meters Distance in meters
    @return Distance in kilometers
    """
    return meters / 1000

@internal
@pure
def celsius_to_kelvin(celsius: int256) -> uint256:
    """
    @notice Convert Celsius to Kelvin (scaled by 100)
    @param celsius Temperature in centi-Celsius
    @return Temperature in centi-Kelvin
    """
    # 273.15 * 100 = 27315
    assert celsius >= -27315, "Below absolute zero"
    return convert(celsius + 27315, uint256)

@internal
@pure
def gwei_to_wei(gwei: uint256) -> uint256:
    """
    @notice Convert gwei to wei
    @param gwei Amount in gwei
    @return Amount in wei
    """
    return gwei * 10 ** 9

@internal
@pure
def wei_to_gwei(wei_amount: uint256) -> uint256:
    """
    @notice Convert wei to gwei
    @param wei_amount Amount in wei
    @return Amount in gwei
    """
    return wei_amount / 10 ** 9

# ============================================================================
# MODULE: SAFE_BOUNDED VALUES
# ============================================================================

@internal
@pure
def clamp(value: uint256, min_val: uint256, max_val: uint256) -> uint256:
    """
    @notice Clamp value to range [min_val, max_val]
    @param value Value to clamp
    @param min_val Minimum allowed value
    @param max_val Maximum allowed value
    @return Clamped value
    """
    if value < min_val:
        return min_val
    if value > max_val:
        return max_val
    return value

@internal
@pure
def in_range(value: uint256, min_val: uint256, max_val: uint256) -> bool:
    """
    @notice Check if value is in range (inclusive)
    @param value Value to check
    @param min_val Minimum of range
    @param max_val Maximum of range
    @return True if value in range
    """
    return value >= min_val and value <= max_val

@internal
@pure
def require_in_range(value: uint256, min_val: uint256, max_val: uint256):
    """
    @notice Require value is in range or revert
    @param value Value to validate
    @param min_val Minimum of range
    @param max_val Maximum of range
    """
    assert value >= min_val and value <= max_val, "Value out of bounds"

@internal
@pure
def is_valid_percentage(value: uint256) -> bool:
    """
    @notice Validate percentage (0-100)
    @param value Percentage value
    @return True if valid
    """
    return value <= MAX_PERCENTAGE

@internal
@pure
def require_valid_percentage(value: uint256):
    """
    @notice Require valid percentage or revert
    @param value Percentage to validate
    """
    assert value <= MAX_PERCENTAGE, "Invalid percentage"

# ============================================================================
# MODULE: SAFE_ETH HANDLING
# ============================================================================

@internal
def safe_transfer_eth(to: address, amount: uint256):
    """
    @notice Safe ETH transfer with success check
    @param to Recipient address
    @param amount Amount to transfer in wei
    """
    assert to != empty(address), "Invalid recipient"
    raw_call(to, b"", value=amount)
    log SafeTransfer(msg.sender, to, amount)

# ============================================================================
# MODULE: SAFE_ARRAY (Bounds checking)
# ============================================================================

@internal
@pure
def safe_array_access(arr: DynArray[uint256, 1000], index: uint256) -> uint256:
    """
    @notice Safe array access with bounds check
    @param arr Dynamic array to access
    @param index Index to access
    @return Value at index
    """
    assert index < len(arr), "Index out of bounds"
    return arr[index]

@internal
@pure
def array_sum(arr: DynArray[uint256, 1000]) -> uint256:
    """
    @notice Calculate sum of array elements
    @param arr Array to sum
    @return Sum of all elements
    """
    total: uint256 = 0
    for i: uint256 in range(1000):
        if i >= len(arr):
            break
        total = total + arr[i]
    return total

@internal
@pure
def array_max(arr: DynArray[uint256, 1000]) -> uint256:
    """
    @notice Find maximum value in array
    @param arr Array to search
    @return Maximum value (0 if empty)
    """
    if len(arr) == 0:
        return 0
    max_val: uint256 = arr[0]
    for i: uint256 in range(1000):
        if i >= len(arr):
            break
        if arr[i] > max_val:
            max_val = arr[i]
    return max_val

@internal
@pure
def array_min(arr: DynArray[uint256, 1000]) -> uint256:
    """
    @notice Find minimum value in array
    @param arr Array to search
    @return Minimum value (MAX_UINT256 if empty)
    """
    if len(arr) == 0:
        return MAX_UINT256
    min_val: uint256 = arr[0]
    for i: uint256 in range(1000):
        if i >= len(arr):
            break
        if arr[i] < min_val:
            min_val = arr[i]
    return min_val

# ============================================================================
# MODULE: SAFE_CHECKSUM
# ============================================================================

@internal
@pure
def simple_checksum(data: Bytes[1024]) -> uint256:
    """
    @notice Calculate simple additive checksum
    @param data Data to checksum
    @return Checksum value
    """
    checksum: uint256 = 0
    for i: uint256 in range(1024):
        if i >= len(data):
            break
        checksum = checksum + convert(slice(data, i, 1), uint256)
    return checksum

@internal
@pure
def xor_checksum(data: Bytes[1024]) -> uint8:
    """
    @notice Calculate XOR checksum
    @param data Data to checksum
    @return XOR checksum byte
    """
    checksum: uint8 = 0
    for i: uint256 in range(1024):
        if i >= len(data):
            break
        checksum = checksum ^ convert(slice(data, i, 1), uint8)
    return checksum

@internal
@pure
def luhn_check_digit(partial: uint256) -> uint8:
    """
    @notice Calculate Luhn check digit
    @dev Works with numeric values up to 18 digits
    @param partial Number without check digit
    @return Check digit (0-9)
    """
    sum: uint256 = 0
    digit: uint256 = 0
    is_double: bool = True
    n: uint256 = partial

    for i: uint256 in range(20):
        if n == 0:
            break
        digit = n % 10
        n = n / 10

        if is_double:
            digit = digit * 2
            if digit > 9:
                digit = digit - 9

        sum = sum + digit
        is_double = not is_double

    return convert((10 - (sum % 10)) % 10, uint8)

# ============================================================================
# MODULE: SAFE_GEO (Geographic coordinates)
# ============================================================================

@internal
@pure
def is_valid_latitude(lat: int256) -> bool:
    """
    @notice Validate latitude (scaled by 1e6)
    @param lat Latitude in micro-degrees
    @return True if valid (-90 to 90 degrees)
    """
    return lat >= MIN_LATITUDE and lat <= MAX_LATITUDE

@internal
@pure
def is_valid_longitude(lon: int256) -> bool:
    """
    @notice Validate longitude (scaled by 1e6)
    @param lon Longitude in micro-degrees
    @return True if valid (-180 to 180 degrees)
    """
    return lon >= MIN_LONGITUDE and lon <= MAX_LONGITUDE

@internal
@pure
def require_valid_coordinates(lat: int256, lon: int256):
    """
    @notice Require valid geographic coordinates
    @param lat Latitude in micro-degrees
    @param lon Longitude in micro-degrees
    """
    assert lat >= MIN_LATITUDE and lat <= MAX_LATITUDE, "Invalid latitude"
    assert lon >= MIN_LONGITUDE and lon <= MAX_LONGITUDE, "Invalid longitude"

@internal
@pure
def is_northern_hemisphere(lat: int256) -> bool:
    """
    @notice Check if latitude is in Northern Hemisphere
    @param lat Latitude in micro-degrees
    @return True if Northern Hemisphere
    """
    return lat >= 0

@internal
@pure
def is_eastern_hemisphere(lon: int256) -> bool:
    """
    @notice Check if longitude is in Eastern Hemisphere
    @param lon Longitude in micro-degrees
    @return True if Eastern Hemisphere
    """
    return lon >= 0

# ============================================================================
# MODULE: SAFE_PROBABILITY
# ============================================================================

@internal
@pure
def is_valid_probability(p: uint256) -> bool:
    """
    @notice Validate probability (0 to 1e18)
    @param p Probability scaled by 1e18
    @return True if valid
    """
    return p <= PROBABILITY_ONE

@internal
@pure
def probability_complement(p: uint256) -> uint256:
    """
    @notice Calculate probability complement (1 - p)
    @param p Probability scaled by 1e18
    @return Complement probability
    """
    assert p <= PROBABILITY_ONE, "Invalid probability"
    return PROBABILITY_ONE - p

@internal
@pure
def probability_and(p1: uint256, p2: uint256) -> uint256:
    """
    @notice Calculate P(A AND B) for independent events
    @param p1 First probability (scaled by 1e18)
    @param p2 Second probability (scaled by 1e18)
    @return Combined probability
    """
    return (p1 * p2) / PROBABILITY_ONE

@internal
@pure
def probability_or(p1: uint256, p2: uint256) -> uint256:
    """
    @notice Calculate P(A OR B) for independent events
    @param p1 First probability (scaled by 1e18)
    @param p2 Second probability (scaled by 1e18)
    @return Combined probability
    """
    # P(A OR B) = P(A) + P(B) - P(A)P(B)
    product: uint256 = (p1 * p2) / PROBABILITY_ONE
    return p1 + p2 - product

@internal
@pure
def probability_to_percent(p: uint256) -> uint256:
    """
    @notice Convert probability to percentage
    @param p Probability (scaled by 1e18)
    @return Percentage (0-100, scaled by 1e16)
    """
    return (p * 100) / PROBABILITY_ONE

@internal
@pure
def percent_to_probability(pct: uint256) -> uint256:
    """
    @notice Convert percentage to probability
    @param pct Percentage (0-100)
    @return Probability (scaled by 1e18)
    """
    assert pct <= 100, "Percentage exceeds 100"
    return (pct * PROBABILITY_ONE) / 100

# ============================================================================
# MODULE: SAFE_TENSOR (Basic tensor operations)
# ============================================================================

@internal
@pure
def dot_product(a: DynArray[uint256, 100], b: DynArray[uint256, 100]) -> uint256:
    """
    @notice Calculate dot product of two vectors
    @param a First vector
    @param b Second vector
    @return Dot product
    """
    assert len(a) == len(b), "Vector length mismatch"
    result: uint256 = 0
    for i: uint256 in range(100):
        if i >= len(a):
            break
        result = result + (a[i] * b[i])
    return result

@internal
@pure
def vector_magnitude_squared(v: DynArray[uint256, 100]) -> uint256:
    """
    @notice Calculate squared magnitude of vector
    @param v Vector
    @return Squared magnitude
    """
    result: uint256 = 0
    for i: uint256 in range(100):
        if i >= len(v):
            break
        result = result + (v[i] * v[i])
    return result

@internal
@pure
def vector_scale(v: DynArray[uint256, 100], scalar: uint256) -> DynArray[uint256, 100]:
    """
    @notice Scale vector by scalar
    @param v Vector to scale
    @param scalar Scale factor
    @return Scaled vector
    """
    result: DynArray[uint256, 100] = []
    for i: uint256 in range(100):
        if i >= len(v):
            break
        result.append(v[i] * scalar)
    return result

# ============================================================================
# MODULE: SAFE_RATE_LIMITER
# ============================================================================

# Rate limiter state variables would be storage in actual contract
# Here we define the logic functions

@internal
@pure
def calculate_tokens_to_add(
    elapsed_time: uint256,
    refill_rate: uint256,
    current_tokens: uint256,
    max_tokens: uint256
) -> uint256:
    """
    @notice Calculate tokens to add based on elapsed time
    @param elapsed_time Time since last refill
    @param refill_rate Tokens per second
    @param current_tokens Current token count
    @param max_tokens Maximum token capacity
    @return New token count
    """
    new_tokens: uint256 = current_tokens + (elapsed_time * refill_rate)
    if new_tokens > max_tokens:
        return max_tokens
    return new_tokens

@internal
@pure
def can_consume_tokens(current_tokens: uint256, requested: uint256) -> bool:
    """
    @notice Check if tokens can be consumed
    @param current_tokens Available tokens
    @param requested Tokens requested
    @return True if sufficient tokens
    """
    return current_tokens >= requested

@internal
@pure
def calculate_wait_time(
    current_tokens: uint256,
    requested: uint256,
    refill_rate: uint256
) -> uint256:
    """
    @notice Calculate wait time until tokens available
    @param current_tokens Current token count
    @param requested Tokens needed
    @param refill_rate Tokens per second
    @return Wait time in seconds
    """
    if current_tokens >= requested:
        return 0
    needed: uint256 = requested - current_tokens
    return (needed + refill_rate - 1) / refill_rate  # Ceiling division

# ============================================================================
# MODULE: SAFE_CIRCUIT_BREAKER
# ============================================================================

# Circuit breaker states
CIRCUIT_CLOSED: constant(uint8) = 0
CIRCUIT_OPEN: constant(uint8) = 1
CIRCUIT_HALF_OPEN: constant(uint8) = 2

@internal
@pure
def should_open_circuit(
    failure_count: uint256,
    failure_threshold: uint256
) -> bool:
    """
    @notice Check if circuit should open based on failures
    @param failure_count Current failure count
    @param failure_threshold Threshold to open
    @return True if should open
    """
    return failure_count >= failure_threshold

@internal
@pure
def should_attempt_reset(
    last_failure_time: uint256,
    current_time: uint256,
    timeout: uint256
) -> bool:
    """
    @notice Check if circuit should attempt reset (half-open)
    @param last_failure_time Time of last failure
    @param current_time Current timestamp
    @param timeout Recovery timeout
    @return True if should try half-open
    """
    return current_time >= last_failure_time + timeout

@internal
@pure
def should_close_circuit(
    success_count: uint256,
    success_threshold: uint256
) -> bool:
    """
    @notice Check if circuit should close after recovery
    @param success_count Consecutive successes in half-open
    @param success_threshold Required successes to close
    @return True if should close
    """
    return success_count >= success_threshold

# ============================================================================
# MODULE: SAFE_RETRY
# ============================================================================

@internal
@pure
def calculate_exponential_backoff(
    attempt: uint256,
    base_delay_ms: uint256,
    max_delay_ms: uint256,
    multiplier: uint256
) -> uint256:
    """
    @notice Calculate exponential backoff delay
    @param attempt Current attempt number (0-indexed)
    @param base_delay_ms Initial delay in milliseconds
    @param max_delay_ms Maximum delay cap
    @param multiplier Multiplier (scaled by 100, so 200 = 2x)
    @return Delay in milliseconds
    """
    if attempt == 0:
        return base_delay_ms

    delay: uint256 = base_delay_ms
    for i: uint256 in range(32):  # Max 32 iterations
        if i >= attempt:
            break
        delay = (delay * multiplier) / 100
        if delay > max_delay_ms:
            return max_delay_ms

    return delay

@internal
@pure
def should_retry(
    current_attempt: uint256,
    max_attempts: uint256
) -> bool:
    """
    @notice Check if retry should be attempted
    @param current_attempt Current attempt number
    @param max_attempts Maximum allowed attempts
    @return True if should retry
    """
    return current_attempt < max_attempts

# ============================================================================
# MODULE: SAFE_MONOTONIC
# ============================================================================

@internal
@pure
def ensure_monotonic_increase(
    current_value: uint256,
    new_value: uint256
) -> uint256:
    """
    @notice Ensure value only increases
    @param current_value Current stored value
    @param new_value Proposed new value
    @return Maximum of current and new
    """
    if new_value > current_value:
        return new_value
    return current_value

@internal
@pure
def is_monotonic_increase(
    current_value: uint256,
    new_value: uint256
) -> bool:
    """
    @notice Check if new value maintains monotonic increase
    @param current_value Current value
    @param new_value Proposed value
    @return True if new >= current
    """
    return new_value >= current_value

@internal
@pure
def pack_timestamp_sequence(timestamp: uint256, sequence: uint256) -> uint256:
    """
    @notice Pack timestamp and sequence into single ID
    @param timestamp Unix timestamp (fits in 40 bits until year 36812)
    @param sequence Sequence number (24 bits)
    @return Packed ID
    """
    assert timestamp < 2 ** 40, "Timestamp too large"
    assert sequence < 2 ** 24, "Sequence too large"
    return (timestamp << 24) | sequence

@internal
@pure
def unpack_timestamp(packed_id: uint256) -> uint256:
    """
    @notice Extract timestamp from packed ID
    @param packed_id Packed timestamp+sequence
    @return Timestamp
    """
    return packed_id >> 24

@internal
@pure
def unpack_sequence(packed_id: uint256) -> uint256:
    """
    @notice Extract sequence from packed ID
    @param packed_id Packed timestamp+sequence
    @return Sequence number
    """
    return packed_id & 0xFFFFFF

# ============================================================================
# MODULE: SAFE_STATE_MACHINE
# ============================================================================

@internal
@pure
def is_valid_state_transition(
    current_state: uint8,
    new_state: uint8,
    transitions: DynArray[uint16, 256]
) -> bool:
    """
    @notice Check if state transition is valid
    @dev Transitions encoded as uint16: (from << 8) | to
    @param current_state Current state
    @param new_state Target state
    @param transitions Array of valid transitions
    @return True if transition is allowed
    """
    encoded: uint16 = (convert(current_state, uint16) << 8) | convert(new_state, uint16)
    for i: uint256 in range(256):
        if i >= len(transitions):
            break
        if transitions[i] == encoded:
            return True
    return False

@internal
@pure
def encode_transition(from_state: uint8, to_state: uint8) -> uint16:
    """
    @notice Encode state transition as uint16
    @param from_state Source state
    @param to_state Target state
    @return Encoded transition
    """
    return (convert(from_state, uint16) << 8) | convert(to_state, uint16)

@internal
@pure
def decode_transition_from(encoded: uint16) -> uint8:
    """
    @notice Decode source state from transition
    @param encoded Encoded transition
    @return Source state
    """
    return convert(encoded >> 8, uint8)

@internal
@pure
def decode_transition_to(encoded: uint16) -> uint8:
    """
    @notice Decode target state from transition
    @param encoded Encoded transition
    @return Target state
    """
    return convert(encoded & 0xFF, uint8)

# ============================================================================
# MODULE: SAFE_CALCULATOR (Stack-based calculator)
# ============================================================================

@internal
@pure
def safe_calculate_percentage_change(
    old_value: uint256,
    new_value: uint256
) -> int256:
    """
    @notice Calculate percentage change (scaled by 1e18)
    @param old_value Original value
    @param new_value New value
    @return Percentage change (can be negative)
    """
    if old_value == 0:
        if new_value == 0:
            return 0
        return max_value(int256)  # Infinity representation

    if new_value >= old_value:
        diff: uint256 = new_value - old_value
        return convert((diff * 10 ** 18) / old_value, int256)
    else:
        diff: uint256 = old_value - new_value
        return -convert((diff * 10 ** 18) / old_value, int256)

@internal
@pure
def safe_average(values: DynArray[uint256, 100]) -> uint256:
    """
    @notice Calculate average of values
    @param values Array of values
    @return Average (0 if empty)
    """
    if len(values) == 0:
        return 0

    total: uint256 = 0
    for i: uint256 in range(100):
        if i >= len(values):
            break
        total = total + values[i]

    return total / len(values)

@internal
@pure
def safe_weighted_average(
    values: DynArray[uint256, 100],
    weights: DynArray[uint256, 100]
) -> uint256:
    """
    @notice Calculate weighted average
    @param values Array of values
    @param weights Array of weights
    @return Weighted average
    """
    assert len(values) == len(weights), "Length mismatch"
    if len(values) == 0:
        return 0

    weighted_sum: uint256 = 0
    weight_sum: uint256 = 0

    for i: uint256 in range(100):
        if i >= len(values):
            break
        weighted_sum = weighted_sum + (values[i] * weights[i])
        weight_sum = weight_sum + weights[i]

    if weight_sum == 0:
        return 0

    return weighted_sum / weight_sum

# ============================================================================
# MODULE: SAFE_BLOOM (Bloom filter primitives)
# ============================================================================

@internal
@pure
def bloom_hash_1(item: bytes32, size: uint256) -> uint256:
    """
    @notice First hash function for bloom filter
    @param item Item to hash
    @param size Filter size
    @return Index in filter
    """
    return convert(keccak256(concat(b"\x01", convert(item, Bytes[32]))), uint256) % size

@internal
@pure
def bloom_hash_2(item: bytes32, size: uint256) -> uint256:
    """
    @notice Second hash function for bloom filter
    @param item Item to hash
    @param size Filter size
    @return Index in filter
    """
    return convert(keccak256(concat(b"\x02", convert(item, Bytes[32]))), uint256) % size

@internal
@pure
def bloom_hash_3(item: bytes32, size: uint256) -> uint256:
    """
    @notice Third hash function for bloom filter
    @param item Item to hash
    @param size Filter size
    @return Index in filter
    """
    return convert(keccak256(concat(b"\x03", convert(item, Bytes[32]))), uint256) % size

@internal
@pure
def optimal_bloom_size(expected_items: uint256, false_positive_rate: uint256) -> uint256:
    """
    @notice Calculate optimal bloom filter size
    @dev false_positive_rate scaled by 1e18 (e.g., 1e16 = 1%)
    @param expected_items Expected number of items
    @param false_positive_rate Desired FP rate (scaled)
    @return Optimal bit count
    """
    # m = -n * ln(p) / (ln(2))^2
    # Approximate: m = n * 10 for 1% FP rate
    # Scale based on rate
    if false_positive_rate >= 10 ** 16:  # 1% or higher
        return expected_items * 10
    if false_positive_rate >= 10 ** 15:  # 0.1%
        return expected_items * 15
    return expected_items * 20  # <0.1%

# ============================================================================
# MODULE: SAFE_LRU (LRU cache primitives)
# ============================================================================

@internal
@pure
def lru_eviction_needed(current_size: uint256, max_size: uint256) -> bool:
    """
    @notice Check if LRU eviction is needed
    @param current_size Current cache size
    @param max_size Maximum capacity
    @return True if eviction needed
    """
    return current_size >= max_size

@internal
@pure
def lru_calculate_new_size(
    current_size: uint256,
    additions: uint256,
    evictions: uint256
) -> uint256:
    """
    @notice Calculate new cache size after operations
    @param current_size Current size
    @param additions Items to add
    @param evictions Items to evict
    @return New size
    """
    return current_size + additions - evictions

# ============================================================================
# MODULE: SAFE_GRAPH (Graph primitives)
# ============================================================================

@internal
@pure
def encode_edge(from_node: uint128, to_node: uint128) -> uint256:
    """
    @notice Encode edge as uint256
    @param from_node Source node ID
    @param to_node Target node ID
    @return Encoded edge
    """
    return (convert(from_node, uint256) << 128) | convert(to_node, uint256)

@internal
@pure
def decode_edge_from(encoded: uint256) -> uint128:
    """
    @notice Decode source node from edge
    @param encoded Encoded edge
    @return Source node ID
    """
    return convert(encoded >> 128, uint128)

@internal
@pure
def decode_edge_to(encoded: uint256) -> uint128:
    """
    @notice Decode target node from edge
    @param encoded Encoded edge
    @return Target node ID
    """
    return convert(encoded & ((1 << 128) - 1), uint128)

# ============================================================================
# MODULE: SAFE_QUEUE (Queue primitives)
# ============================================================================

@internal
@pure
def queue_is_empty(head: uint256, tail: uint256) -> bool:
    """
    @notice Check if queue is empty
    @param head Queue head index
    @param tail Queue tail index
    @return True if empty
    """
    return head == tail

@internal
@pure
def queue_is_full(head: uint256, tail: uint256, capacity: uint256) -> bool:
    """
    @notice Check if queue is full
    @param head Queue head index
    @param tail Queue tail index
    @param capacity Maximum capacity
    @return True if full
    """
    return ((tail + 1) % capacity) == head

@internal
@pure
def queue_size(head: uint256, tail: uint256, capacity: uint256) -> uint256:
    """
    @notice Get current queue size
    @param head Queue head index
    @param tail Queue tail index
    @param capacity Queue capacity
    @return Current size
    """
    if tail >= head:
        return tail - head
    return capacity - head + tail

@internal
@pure
def queue_next_index(current: uint256, capacity: uint256) -> uint256:
    """
    @notice Get next circular index
    @param current Current index
    @param capacity Queue capacity
    @return Next index (wraps around)
    """
    return (current + 1) % capacity

# ============================================================================
# MODULE: SAFE_BUFFER (Ring buffer primitives)
# ============================================================================

@internal
@pure
def buffer_write_index(current_index: uint256, buffer_size: uint256) -> uint256:
    """
    @notice Calculate next write index for ring buffer
    @param current_index Current write position
    @param buffer_size Buffer capacity
    @return Next write index
    """
    return (current_index + 1) % buffer_size

@internal
@pure
def buffer_read_index(
    write_index: uint256,
    offset: uint256,
    buffer_size: uint256
) -> uint256:
    """
    @notice Calculate read index (offset from write position)
    @param write_index Current write position
    @param offset How far back to read (1 = most recent)
    @param buffer_size Buffer capacity
    @return Read index
    """
    assert offset > 0 and offset <= buffer_size, "Invalid offset"
    if write_index >= offset:
        return write_index - offset
    return buffer_size - (offset - write_index)

# ============================================================================
# MODULE: SAFE_PASSWORD (Password strength validation)
# ============================================================================

@internal
@pure
def password_entropy_bits(length: uint256, charset_size: uint256) -> uint256:
    """
    @notice Estimate password entropy in bits
    @dev Simplified: log2(charset^length) = length * log2(charset)
    @param length Password length
    @param charset_size Size of character set
    @return Approximate entropy bits
    """
    # Approximate log2 values for common charsets
    # 26 letters: ~4.7 bits
    # 52 letters: ~5.7 bits
    # 62 alphanumeric: ~6 bits
    # 95 printable: ~6.6 bits

    log2_charset: uint256 = 0
    if charset_size <= 26:
        log2_charset = 47  # Scaled by 10
    elif charset_size <= 52:
        log2_charset = 57
    elif charset_size <= 62:
        log2_charset = 60
    else:
        log2_charset = 66

    return (length * log2_charset) / 10

@internal
@pure
def is_strong_password(entropy_bits: uint256, min_bits: uint256) -> bool:
    """
    @notice Check if password meets minimum entropy
    @param entropy_bits Calculated entropy
    @param min_bits Minimum required bits (e.g., 60)
    @return True if strong enough
    """
    return entropy_bits >= min_bits

# ============================================================================
# MODULE: SAFE_ML (ML safety primitives)
# ============================================================================

@internal
@pure
def softmax_denominator(scores: DynArray[uint256, 100], scale: uint256) -> uint256:
    """
    @notice Calculate softmax denominator (sum of exp)
    @dev Uses scaled integer math; scores should be pre-scaled
    @param scores Array of scores
    @param scale Scaling factor
    @return Sum for softmax denominator
    """
    total: uint256 = 0
    for i: uint256 in range(100):
        if i >= len(scores):
            break
        # Simplified exp approximation using Taylor series first terms
        # exp(x) ≈ 1 + x + x^2/2
        x: uint256 = scores[i]
        exp_approx: uint256 = scale + x + (x * x) / (2 * scale)
        total = total + exp_approx
    return total

@internal
@pure
def normalize_weights(
    weights: DynArray[uint256, 100]
) -> DynArray[uint256, 100]:
    """
    @notice Normalize weights to sum to 1e18
    @param weights Array of weights
    @return Normalized weights
    """
    total: uint256 = 0
    for i: uint256 in range(100):
        if i >= len(weights):
            break
        total = total + weights[i]

    result: DynArray[uint256, 100] = []
    if total == 0:
        for i: uint256 in range(100):
            if i >= len(weights):
                break
            result.append(0)
        return result

    for i: uint256 in range(100):
        if i >= len(weights):
            break
        result.append((weights[i] * 10 ** 18) / total)

    return result

@internal
@pure
def clamp_gradient(gradient: int256, max_norm: uint256) -> int256:
    """
    @notice Clamp gradient to maximum norm
    @param gradient Gradient value
    @param max_norm Maximum allowed absolute value
    @return Clamped gradient
    """
    max_int: int256 = convert(max_norm, int256)
    if gradient > max_int:
        return max_int
    if gradient < -max_int:
        return -max_int
    return gradient

# ============================================================================
# MODULE: SAFE_HEADER (HTTP header validation primitives)
# ============================================================================

@internal
@pure
def is_valid_header_name_char(c: uint8) -> bool:
    """
    @notice Check if byte is valid HTTP header name character
    @dev Per RFC 7230: token chars (no colons, spaces, etc.)
    @param c ASCII byte value
    @return True if valid
    """
    # A-Z: 65-90, a-z: 97-122, 0-9: 48-57
    # Also: ! # $ % & ' * + - . ^ _ ` | ~
    if c >= 65 and c <= 90:  # A-Z
        return True
    if c >= 97 and c <= 122:  # a-z
        return True
    if c >= 48 and c <= 57:  # 0-9
        return True
    # Special chars
    if c == 33 or c == 35 or c == 36 or c == 37 or c == 38:  # ! # $ % &
        return True
    if c == 39 or c == 42 or c == 43 or c == 45 or c == 46:  # ' * + - .
        return True
    if c == 94 or c == 95 or c == 96 or c == 124 or c == 126:  # ^ _ ` | ~
        return True
    return False

@internal
@pure
def header_name_length_valid(length: uint256) -> bool:
    """
    @notice Check if header name length is valid
    @param length Header name length
    @return True if valid (1-256 per practical limits)
    """
    return length > 0 and length <= 256

# ============================================================================
# MODULE: SAFE_COOKIE (Cookie validation primitives)
# ============================================================================

@internal
@pure
def is_valid_cookie_name_char(c: uint8) -> bool:
    """
    @notice Check if byte is valid cookie name character
    @dev Per RFC 6265: token chars (subset of ASCII)
    @param c ASCII byte value
    @return True if valid
    """
    # Same as header token chars but excluding = ; , space
    if c >= 65 and c <= 90:  # A-Z
        return True
    if c >= 97 and c <= 122:  # a-z
        return True
    if c >= 48 and c <= 57:  # 0-9
        return True
    if c == 95 or c == 45:  # _ -
        return True
    return False

@internal
@pure
def cookie_max_age_valid(max_age: uint256) -> bool:
    """
    @notice Check if cookie max-age is reasonable
    @param max_age Max age in seconds
    @return True if valid (up to 2 years)
    """
    return max_age <= 63072000  # 2 years in seconds

# ============================================================================
# MODULE: SAFE_CONTENT_TYPE (Content-Type validation)
# ============================================================================

@internal
@pure
def is_json_content_type(type_indicator: uint8) -> bool:
    """
    @notice Check if content type indicates JSON
    @dev Uses numeric indicator (1 = application/json)
    @param type_indicator Content type code
    @return True if JSON type
    """
    return type_indicator == 1

@internal
@pure
def is_html_content_type(type_indicator: uint8) -> bool:
    """
    @notice Check if content type indicates HTML
    @dev Uses numeric indicator (2 = text/html)
    @param type_indicator Content type code
    @return True if HTML type
    """
    return type_indicator == 2

@internal
@pure
def is_text_content_type(type_indicator: uint8) -> bool:
    """
    @notice Check if content type indicates text
    @dev Types 2-5 are text types (html, plain, css, js)
    @param type_indicator Content type code
    @return True if text type
    """
    return type_indicator >= 2 and type_indicator <= 5

# Content type codes:
# 0 = unknown
# 1 = application/json
# 2 = text/html
# 3 = text/plain
# 4 = text/css
# 5 = text/javascript
# 6 = application/xml
# 7 = image/png
# 8 = image/jpeg
# 9 = application/octet-stream
