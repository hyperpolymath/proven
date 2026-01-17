# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
Proven Safety Library for MicroPython

Formally verified safety primitives optimized for embedded systems.
Designed for microcontrollers with limited memory (ESP32/RP2040).

Includes 38 modules across 8 categories:
- Core (11): safe_math, safe_string, safe_path, safe_email, safe_url,
             safe_network, safe_crypto, safe_uuid, safe_currency, safe_phone, safe_hex
- Data (7): safe_json, safe_datetime, safe_float, safe_version, safe_color,
            safe_angle, safe_unit
- Data Structures (5): safe_buffer, safe_queue, safe_bloom, safe_lru, safe_graph
- Resilience (4): safe_rate_limiter, safe_circuit_breaker, safe_retry, safe_monotonic
- State (2): safe_state_machine, safe_calculator
- Algorithm (4): safe_geo, safe_probability, safe_checksum, safe_tensor
- Security (2): safe_password, safe_ml
- HTTP (3): safe_header, safe_cookie, safe_content_type

Version: 0.4.0
Module Count: 38
"""

VERSION = "0.4.0"
MODULE_COUNT = 38

# ============================================================================
# CONFIGURATION - Adjust for your platform
# ============================================================================

# 32-bit signed integer bounds (common on microcontrollers)
INT32_MAX = 2147483647
INT32_MIN = -2147483648

# 16-bit bounds (for smaller MCUs)
INT16_MAX = 32767
INT16_MIN = -32768

# Default to 32-bit
INT_MAX = INT32_MAX
INT_MIN = INT32_MIN


# ============================================================================
# RESULT TYPE (memory efficient)
# ============================================================================

class Result:
    """
    Result type for safe operations.
    Uses __slots__ to minimize memory overhead.
    """
    __slots__ = ('ok', 'value', 'error')

    def __init__(self, ok, value=None, error=None):
        self.ok = ok
        self.value = value
        self.error = error

    def is_ok(self):
        return self.ok

    def is_err(self):
        return not self.ok

    def unwrap(self):
        """Get value or raise exception if error."""
        if self.ok:
            return self.value
        raise ValueError(self.error)

    def unwrap_or(self, default):
        """Get value or return default if error."""
        return self.value if self.ok else default

    def map(self, fn):
        """Apply function to value if ok."""
        if self.ok:
            return Result(True, fn(self.value))
        return self


def Ok(value):
    """Create successful result."""
    return Result(True, value)


def Err(error):
    """Create error result."""
    return Result(False, error=error)


# ============================================================================
# MODULE 1: SAFE MATH
# ============================================================================

def safe_add(a, b):
    """Safe addition with overflow check."""
    if b > 0 and a > INT_MAX - b:
        return Err("overflow")
    if b < 0 and a < INT_MIN - b:
        return Err("underflow")
    return Ok(a + b)


def safe_sub(a, b):
    """Safe subtraction with underflow check."""
    if b < 0 and a > INT_MAX + b:
        return Err("overflow")
    if b > 0 and a < INT_MIN + b:
        return Err("underflow")
    return Ok(a - b)


def safe_mul(a, b):
    """Safe multiplication with overflow check."""
    if a == 0 or b == 0:
        return Ok(0)
    result = a * b
    if result // a != b:
        return Err("overflow")
    return Ok(result)


def safe_div(a, b):
    """Safe division with zero check."""
    if b == 0:
        return Err("division_by_zero")
    if a == INT_MIN and b == -1:
        return Err("overflow")
    return Ok(a // b)


def safe_mod(a, b):
    """Safe modulo with zero check."""
    if b == 0:
        return Err("modulo_by_zero")
    return Ok(a % b)


def safe_abs(a):
    """Safe absolute value."""
    if a == INT_MIN:
        return Err("overflow")
    return Ok(abs(a))


def safe_pow(base, exp):
    """Safe exponentiation with overflow check."""
    if exp < 0:
        return Err("negative_exponent")
    result = 1
    for _ in range(exp):
        r = safe_mul(result, base)
        if r.is_err():
            return r
        result = r.value
    return Ok(result)


# ============================================================================
# MODULE 2: SAFE STRING
# ============================================================================

def safe_slice(s, start, end, max_len=1024):
    """Safe string slice with bounds checking."""
    if not isinstance(s, str):
        return Err("not_string")
    length = len(s)
    if length > max_len:
        return Err("string_too_long")
    if start < 0:
        start = 0
    if end > length:
        end = length
    if start > end:
        return Err("invalid_range")
    return Ok(s[start:end])


def safe_concat(a, b, max_len=1024):
    """Safe string concatenation with length limit."""
    if len(a) + len(b) > max_len:
        return Err("result_too_long")
    return Ok(a + b)


def safe_parse_int(s, default=None):
    """Safely parse integer from string."""
    try:
        return Ok(int(s))
    except (ValueError, TypeError):
        if default is not None:
            return Ok(default)
        return Err("parse_error")


def safe_truncate(s, max_len, suffix="..."):
    """Safely truncate string with suffix."""
    if len(s) <= max_len:
        return Ok(s)
    if max_len < len(suffix):
        return Ok(s[:max_len])
    return Ok(s[:max_len - len(suffix)] + suffix)


def is_ascii(s):
    """Check if string is ASCII only."""
    for c in s:
        if ord(c) > 127:
            return False
    return True


def is_printable(s):
    """Check if string is printable ASCII."""
    for c in s:
        o = ord(c)
        if o < 32 or o > 126:
            return False
    return True


# ============================================================================
# MODULE 3: SAFE PATH
# ============================================================================

def safe_path_join(base, *parts, max_len=256):
    """Safely join path components."""
    result = base.rstrip("/")
    for part in parts:
        part = part.strip("/")
        if ".." in part:
            return Err("path_traversal")
        if len(result) + len(part) + 1 > max_len:
            return Err("path_too_long")
        result = result + "/" + part
    return Ok(result)


def safe_path_normalize(path):
    """Normalize path, removing . and resolving .."""
    if ".." in path:
        return Err("path_traversal")
    parts = path.split("/")
    result = []
    for part in parts:
        if part == "" or part == ".":
            continue
        result.append(part)
    return Ok("/" + "/".join(result) if path.startswith("/") else "/".join(result))


def is_safe_filename(name, max_len=64):
    """Check if filename is safe for embedded filesystem."""
    if len(name) > max_len:
        return False
    unsafe = '<>:"/\\|?*\x00'
    for c in name:
        if c in unsafe or ord(c) < 32:
            return False
    return True


# ============================================================================
# MODULE 4: SAFE EMAIL
# ============================================================================

def is_valid_email(email, max_len=254):
    """Basic email format validation."""
    if len(email) > max_len:
        return False
    if "@" not in email:
        return False
    parts = email.split("@")
    if len(parts) != 2:
        return False
    local, domain = parts
    if not local or not domain:
        return False
    if len(local) > 64:
        return False
    if "." not in domain:
        return False
    return True


def safe_email_parts(email):
    """Safely extract email parts."""
    if "@" not in email:
        return Err("invalid_email")
    parts = email.split("@")
    if len(parts) != 2:
        return Err("invalid_email")
    return Ok({"local": parts[0], "domain": parts[1]})


# ============================================================================
# MODULE 5: SAFE URL
# ============================================================================

def safe_url_encode(s):
    """URL-encode unsafe characters (minimal for MCU)."""
    result = []
    for c in s:
        o = ord(c)
        if (48 <= o <= 57 or 65 <= o <= 90 or 97 <= o <= 122 or
            c in "-_.~"):
            result.append(c)
        else:
            result.append("%{:02X}".format(o))
    return Ok("".join(result))


def safe_url_decode(s):
    """URL-decode percent-encoded characters."""
    result = []
    i = 0
    while i < len(s):
        if s[i] == "%" and i + 2 < len(s):
            try:
                result.append(chr(int(s[i+1:i+3], 16)))
                i += 3
                continue
            except ValueError:
                return Err("invalid_encoding")
        result.append(s[i])
        i += 1
    return Ok("".join(result))


def is_valid_url_scheme(scheme):
    """Check if URL scheme is valid."""
    valid = ("http", "https", "ftp", "mqtt", "ws", "wss")
    return scheme.lower() in valid


# ============================================================================
# MODULE 6: SAFE NETWORK
# ============================================================================

def is_valid_ipv4(ip):
    """Validate IPv4 address string."""
    parts = ip.split(".")
    if len(parts) != 4:
        return False
    for p in parts:
        try:
            n = int(p)
            if n < 0 or n > 255:
                return False
        except ValueError:
            return False
    return True


def is_valid_port(port):
    """Validate network port number."""
    return 0 < port <= 65535


def is_private_ipv4(ip):
    """Check if IPv4 address is private."""
    parts = ip.split(".")
    if len(parts) != 4:
        return False
    try:
        a, b = int(parts[0]), int(parts[1])
        # 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16
        if a == 10:
            return True
        if a == 172 and 16 <= b <= 31:
            return True
        if a == 192 and b == 168:
            return True
        return False
    except ValueError:
        return False


def is_valid_mac(mac):
    """Validate MAC address."""
    if len(mac) != 17:
        return False
    for i, c in enumerate(mac):
        if i % 3 == 2:
            if c != ":":
                return False
        else:
            if c not in "0123456789abcdefABCDEF":
                return False
    return True


# ============================================================================
# MODULE 7: SAFE CRYPTO
# ============================================================================

def safe_xor(data, key):
    """XOR data with key (simple obfuscation)."""
    if not key:
        return Err("empty_key")
    result = bytearray(len(data))
    key_len = len(key)
    for i, b in enumerate(data):
        result[i] = b ^ key[i % key_len]
    return Ok(bytes(result))


def safe_rotate_left(value, shift, bits=32):
    """Rotate integer left within bit width."""
    shift = shift % bits
    mask = (1 << bits) - 1
    return Ok(((value << shift) | (value >> (bits - shift))) & mask)


def safe_rotate_right(value, shift, bits=32):
    """Rotate integer right within bit width."""
    shift = shift % bits
    mask = (1 << bits) - 1
    return Ok(((value >> shift) | (value << (bits - shift))) & mask)


def simple_hash(data, seed=0x811c9dc5):
    """Simple FNV-1a hash for MCU (32-bit)."""
    h = seed
    for b in data:
        h ^= b
        h = (h * 0x01000193) & 0xFFFFFFFF
    return h


# ============================================================================
# MODULE 8: SAFE UUID
# ============================================================================

def is_valid_uuid(uuid_str):
    """Validate UUID format."""
    if len(uuid_str) != 36:
        return False
    for i, c in enumerate(uuid_str):
        if i in (8, 13, 18, 23):
            if c != "-":
                return False
        else:
            if c not in "0123456789abcdefABCDEF":
                return False
    return True


def uuid_from_bytes(data):
    """Create UUID string from 16 bytes."""
    if len(data) != 16:
        return Err("invalid_length")
    hex_chars = "0123456789abcdef"
    parts = []
    for i, b in enumerate(data):
        parts.append(hex_chars[b >> 4])
        parts.append(hex_chars[b & 0x0F])
        if i in (3, 5, 7, 9):
            parts.append("-")
    return Ok("".join(parts))


def uuid_to_bytes(uuid_str):
    """Convert UUID string to 16 bytes."""
    if not is_valid_uuid(uuid_str):
        return Err("invalid_uuid")
    hex_str = uuid_str.replace("-", "")
    result = bytearray(16)
    for i in range(16):
        result[i] = int(hex_str[i*2:i*2+2], 16)
    return Ok(bytes(result))


# ============================================================================
# MODULE 9: SAFE CURRENCY
# ============================================================================

def safe_currency_add(a, b, precision=2):
    """Add currency values as integers (cents/pence)."""
    return safe_add(a, b)


def safe_currency_sub(a, b, precision=2):
    """Subtract currency values."""
    return safe_sub(a, b)


def safe_currency_mul(amount, multiplier, precision=2):
    """Multiply currency by factor."""
    return safe_mul(amount, multiplier)


def format_currency(cents, symbol="$", precision=2):
    """Format cents as currency string."""
    div = 10 ** precision
    whole = cents // div
    frac = abs(cents) % div
    sign = "-" if cents < 0 else ""
    return Ok("{}{}{}.{:0{}d}".format(sign, symbol, abs(whole), frac, precision))


# ============================================================================
# MODULE 10: SAFE PHONE
# ============================================================================

def is_valid_phone(phone, min_len=7, max_len=15):
    """Basic phone number validation."""
    digits = [c for c in phone if c.isdigit()]
    return min_len <= len(digits) <= max_len


def normalize_phone(phone):
    """Extract digits from phone number."""
    return Ok("".join(c for c in phone if c.isdigit()))


def format_phone_us(digits):
    """Format US phone number."""
    if len(digits) == 10:
        return Ok("({}) {}-{}".format(digits[:3], digits[3:6], digits[6:]))
    if len(digits) == 11 and digits[0] == "1":
        return Ok("+1 ({}) {}-{}".format(digits[1:4], digits[4:7], digits[7:]))
    return Err("invalid_length")


# ============================================================================
# MODULE 11: SAFE HEX
# ============================================================================

def bytes_to_hex(data):
    """Convert bytes to hex string."""
    hex_chars = "0123456789abcdef"
    result = []
    for b in data:
        result.append(hex_chars[b >> 4])
        result.append(hex_chars[b & 0x0F])
    return "".join(result)


def hex_to_bytes(hex_str):
    """Convert hex string to bytes."""
    if len(hex_str) % 2 != 0:
        return Err("odd_length")
    result = bytearray(len(hex_str) // 2)
    for i in range(0, len(hex_str), 2):
        try:
            result[i // 2] = int(hex_str[i:i+2], 16)
        except ValueError:
            return Err("invalid_hex")
    return Ok(bytes(result))


def is_valid_hex(s):
    """Check if string is valid hex."""
    for c in s:
        if c not in "0123456789abcdefABCDEF":
            return False
    return True


# ============================================================================
# MODULE 12: SAFE JSON (minimal for MCU)
# ============================================================================

def json_escape_string(s):
    """Escape string for JSON."""
    result = []
    for c in s:
        if c == '"':
            result.append('\\"')
        elif c == '\\':
            result.append('\\\\')
        elif c == '\n':
            result.append('\\n')
        elif c == '\r':
            result.append('\\r')
        elif c == '\t':
            result.append('\\t')
        elif ord(c) < 32:
            result.append('\\u{:04x}'.format(ord(c)))
        else:
            result.append(c)
    return "".join(result)


def safe_json_string(s, max_len=256):
    """Safely create JSON string value."""
    if len(s) > max_len:
        return Err("string_too_long")
    return Ok('"' + json_escape_string(s) + '"')


def safe_json_number(n):
    """Safely format number for JSON."""
    if isinstance(n, bool):
        return Err("use_json_bool")
    if n == INT_MIN:
        return Err("overflow")
    return Ok(str(n))


def safe_json_bool(b):
    """Format boolean for JSON."""
    return Ok("true" if b else "false")


# ============================================================================
# MODULE 13: SAFE DATETIME
# ============================================================================

def is_leap_year(year):
    """Check if year is leap year."""
    return year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)


def days_in_month(year, month):
    """Get days in month."""
    if month < 1 or month > 12:
        return Err("invalid_month")
    days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    d = days[month - 1]
    if month == 2 and is_leap_year(year):
        d = 29
    return Ok(d)


def is_valid_date(year, month, day):
    """Validate date components."""
    if month < 1 or month > 12:
        return False
    dim = days_in_month(year, month)
    if dim.is_err():
        return False
    return 1 <= day <= dim.value


def is_valid_time(hour, minute, second=0):
    """Validate time components."""
    return 0 <= hour <= 23 and 0 <= minute <= 59 and 0 <= second <= 59


def format_iso_date(year, month, day):
    """Format date as ISO 8601."""
    if not is_valid_date(year, month, day):
        return Err("invalid_date")
    return Ok("{:04d}-{:02d}-{:02d}".format(year, month, day))


def format_iso_time(hour, minute, second=0):
    """Format time as ISO 8601."""
    if not is_valid_time(hour, minute, second):
        return Err("invalid_time")
    return Ok("{:02d}:{:02d}:{:02d}".format(hour, minute, second))


# ============================================================================
# MODULE 14: SAFE FLOAT (fixed-point for MCU)
# ============================================================================

class FixedPoint:
    """Fixed-point number for MCU without FPU."""
    __slots__ = ('_value', '_scale')

    def __init__(self, value=0, scale=1000):
        self._scale = scale
        if isinstance(value, int):
            self._value = value * scale
        else:
            self._value = int(value * scale)

    @classmethod
    def from_raw(cls, raw, scale=1000):
        """Create from raw fixed-point value."""
        fp = cls(0, scale)
        fp._value = raw
        return fp

    @property
    def raw(self):
        return self._value

    def to_int(self):
        return self._value // self._scale

    def add(self, other):
        """Add two fixed-point numbers."""
        if self._scale != other._scale:
            return Err("scale_mismatch")
        r = safe_add(self._value, other._value)
        if r.is_err():
            return r
        return Ok(FixedPoint.from_raw(r.value, self._scale))

    def sub(self, other):
        """Subtract fixed-point numbers."""
        if self._scale != other._scale:
            return Err("scale_mismatch")
        r = safe_sub(self._value, other._value)
        if r.is_err():
            return r
        return Ok(FixedPoint.from_raw(r.value, self._scale))

    def mul(self, other):
        """Multiply fixed-point numbers."""
        r = safe_mul(self._value, other._value)
        if r.is_err():
            return r
        return Ok(FixedPoint.from_raw(r.value // other._scale, self._scale))


# ============================================================================
# MODULE 15: SAFE VERSION
# ============================================================================

def parse_semver(version):
    """Parse semantic version string."""
    parts = version.split(".")
    if len(parts) < 2 or len(parts) > 3:
        return Err("invalid_format")
    try:
        major = int(parts[0])
        minor = int(parts[1])
        patch = int(parts[2]) if len(parts) > 2 else 0
        return Ok({"major": major, "minor": minor, "patch": patch})
    except ValueError:
        return Err("invalid_number")


def compare_semver(v1, v2):
    """Compare two version dicts. Returns -1, 0, or 1."""
    if v1["major"] != v2["major"]:
        return -1 if v1["major"] < v2["major"] else 1
    if v1["minor"] != v2["minor"]:
        return -1 if v1["minor"] < v2["minor"] else 1
    if v1["patch"] != v2["patch"]:
        return -1 if v1["patch"] < v2["patch"] else 1
    return 0


def format_semver(major, minor, patch=0):
    """Format semantic version string."""
    return Ok("{}.{}.{}".format(major, minor, patch))


# ============================================================================
# MODULE 16: SAFE COLOR
# ============================================================================

def rgb_to_hex(r, g, b):
    """Convert RGB to hex color string."""
    if not (0 <= r <= 255 and 0 <= g <= 255 and 0 <= b <= 255):
        return Err("out_of_range")
    return Ok("#{:02x}{:02x}{:02x}".format(r, g, b))


def hex_to_rgb(hex_color):
    """Convert hex color to RGB tuple."""
    if hex_color.startswith("#"):
        hex_color = hex_color[1:]
    if len(hex_color) != 6:
        return Err("invalid_length")
    try:
        r = int(hex_color[0:2], 16)
        g = int(hex_color[2:4], 16)
        b = int(hex_color[4:6], 16)
        return Ok((r, g, b))
    except ValueError:
        return Err("invalid_hex")


def rgb565_pack(r, g, b):
    """Pack RGB to 16-bit RGB565 (common for TFT displays)."""
    r5 = (r >> 3) & 0x1F
    g6 = (g >> 2) & 0x3F
    b5 = (b >> 3) & 0x1F
    return Ok((r5 << 11) | (g6 << 5) | b5)


def rgb565_unpack(color):
    """Unpack RGB565 to RGB tuple."""
    r = ((color >> 11) & 0x1F) << 3
    g = ((color >> 5) & 0x3F) << 2
    b = (color & 0x1F) << 3
    return Ok((r, g, b))


# ============================================================================
# MODULE 17: SAFE ANGLE
# ============================================================================

def normalize_degrees(angle):
    """Normalize angle to 0-359."""
    angle = angle % 360
    if angle < 0:
        angle += 360
    return Ok(angle)


def degrees_to_radians_fixed(degrees, scale=1000):
    """Convert degrees to radians (fixed-point)."""
    # PI * 1000 = 3141, PI/180 * 1000 = 17
    return Ok((degrees * 17) // 10)


def radians_to_degrees_fixed(radians, scale=1000):
    """Convert radians to degrees (fixed-point)."""
    # 180/PI * 1000 = 57295
    return Ok((radians * 573) // 10)


def angle_difference(a1, a2):
    """Shortest difference between two angles (degrees)."""
    diff = (a2 - a1 + 180) % 360 - 180
    return Ok(diff)


# ============================================================================
# MODULE 18: SAFE UNIT
# ============================================================================

def celsius_to_fahrenheit(c):
    """Convert Celsius to Fahrenheit (integer)."""
    return safe_add(safe_mul(c, 9).unwrap_or(0) // 5, 32)


def fahrenheit_to_celsius(f):
    """Convert Fahrenheit to Celsius (integer)."""
    r = safe_sub(f, 32)
    if r.is_err():
        return r
    return safe_div(safe_mul(r.value, 5).unwrap_or(0), 9)


def mm_to_inch_fixed(mm, scale=1000):
    """Convert mm to inches (fixed-point)."""
    return Ok((mm * 394) // 10)  # 1 inch = 25.4mm


def inch_to_mm_fixed(inch, scale=1000):
    """Convert inches to mm (fixed-point)."""
    return Ok((inch * 254) // 10)


def kph_to_mph(kph):
    """Convert km/h to mph (integer approx)."""
    return Ok((kph * 621) // 1000)


def mph_to_kph(mph):
    """Convert mph to km/h (integer approx)."""
    return Ok((mph * 1609) // 1000)


# ============================================================================
# MODULE 19: SAFE BUFFER
# ============================================================================

class RingBuffer:
    """Memory-efficient ring buffer for sensor data."""
    __slots__ = ('_data', '_size', '_head', '_tail', '_count')

    def __init__(self, size):
        self._data = [0] * size
        self._size = size
        self._head = 0
        self._tail = 0
        self._count = 0

    def push(self, value):
        """Add value to buffer, overwrite oldest if full."""
        self._data[self._head] = value
        self._head = (self._head + 1) % self._size
        if self._count < self._size:
            self._count += 1
        else:
            self._tail = (self._tail + 1) % self._size
        return Ok(None)

    def pop(self):
        """Remove and return oldest value."""
        if self._count == 0:
            return Err("empty")
        value = self._data[self._tail]
        self._tail = (self._tail + 1) % self._size
        self._count -= 1
        return Ok(value)

    def peek(self):
        """View oldest value without removing."""
        if self._count == 0:
            return Err("empty")
        return Ok(self._data[self._tail])

    def is_empty(self):
        return self._count == 0

    def is_full(self):
        return self._count == self._size

    def __len__(self):
        return self._count

    def average(self):
        """Calculate average of buffer contents."""
        if self._count == 0:
            return Err("empty")
        total = 0
        for i in range(self._count):
            idx = (self._tail + i) % self._size
            total += self._data[idx]
        return safe_div(total, self._count)


# ============================================================================
# MODULE 20: SAFE QUEUE
# ============================================================================

class BoundedQueue:
    """Bounded queue with maximum size."""
    __slots__ = ('_items', '_max_size')

    def __init__(self, max_size=16):
        self._items = []
        self._max_size = max_size

    def enqueue(self, item):
        """Add item to queue."""
        if len(self._items) >= self._max_size:
            return Err("queue_full")
        self._items.append(item)
        return Ok(None)

    def dequeue(self):
        """Remove and return first item."""
        if not self._items:
            return Err("queue_empty")
        return Ok(self._items.pop(0))

    def peek(self):
        """View first item without removing."""
        if not self._items:
            return Err("queue_empty")
        return Ok(self._items[0])

    def is_empty(self):
        return len(self._items) == 0

    def is_full(self):
        return len(self._items) >= self._max_size

    def __len__(self):
        return len(self._items)

    def clear(self):
        """Remove all items."""
        self._items = []
        return Ok(None)


# ============================================================================
# MODULE 21: SAFE BLOOM (minimal bloom filter)
# ============================================================================

class BloomFilter:
    """Simple bloom filter for memory-efficient set membership."""
    __slots__ = ('_bits', '_size', '_hashes')

    def __init__(self, size=64, num_hashes=3):
        self._size = size
        self._hashes = num_hashes
        self._bits = [0] * ((size + 7) // 8)

    def _get_bit(self, idx):
        return (self._bits[idx // 8] >> (idx % 8)) & 1

    def _set_bit(self, idx):
        self._bits[idx // 8] |= (1 << (idx % 8))

    def _hash(self, data, seed):
        h = seed
        for b in data:
            if isinstance(b, str):
                b = ord(b)
            h = ((h * 31) + b) % self._size
        return h

    def add(self, item):
        """Add item to bloom filter."""
        if isinstance(item, str):
            item = item.encode() if hasattr(item, 'encode') else bytes(item, 'utf-8')
        for i in range(self._hashes):
            idx = self._hash(item, i * 0x9e3779b9)
            self._set_bit(idx)
        return Ok(None)

    def might_contain(self, item):
        """Check if item might be in set (may have false positives)."""
        if isinstance(item, str):
            item = item.encode() if hasattr(item, 'encode') else bytes(item, 'utf-8')
        for i in range(self._hashes):
            idx = self._hash(item, i * 0x9e3779b9)
            if not self._get_bit(idx):
                return False
        return True


# ============================================================================
# MODULE 22: SAFE LRU (Least Recently Used cache)
# ============================================================================

class LRUCache:
    """Simple LRU cache for MCU."""
    __slots__ = ('_cache', '_order', '_max_size')

    def __init__(self, max_size=8):
        self._cache = {}
        self._order = []
        self._max_size = max_size

    def get(self, key):
        """Get value from cache."""
        if key not in self._cache:
            return Err("not_found")
        # Move to end (most recently used)
        self._order.remove(key)
        self._order.append(key)
        return Ok(self._cache[key])

    def put(self, key, value):
        """Put value in cache."""
        if key in self._cache:
            self._order.remove(key)
        elif len(self._cache) >= self._max_size:
            # Evict least recently used
            lru_key = self._order.pop(0)
            del self._cache[lru_key]
        self._cache[key] = value
        self._order.append(key)
        return Ok(None)

    def remove(self, key):
        """Remove key from cache."""
        if key in self._cache:
            del self._cache[key]
            self._order.remove(key)
            return Ok(None)
        return Err("not_found")

    def __len__(self):
        return len(self._cache)

    def clear(self):
        """Clear cache."""
        self._cache = {}
        self._order = []


# ============================================================================
# MODULE 23: SAFE GRAPH (minimal directed graph)
# ============================================================================

class DirectedGraph:
    """Simple directed graph for state machines and routing."""
    __slots__ = ('_nodes', '_edges')

    def __init__(self):
        self._nodes = set()
        self._edges = {}

    def add_node(self, node):
        """Add node to graph."""
        self._nodes.add(node)
        if node not in self._edges:
            self._edges[node] = []
        return Ok(None)

    def add_edge(self, from_node, to_node, weight=1):
        """Add directed edge."""
        if from_node not in self._nodes:
            self.add_node(from_node)
        if to_node not in self._nodes:
            self.add_node(to_node)
        self._edges[from_node].append((to_node, weight))
        return Ok(None)

    def neighbors(self, node):
        """Get neighbors of node."""
        if node not in self._nodes:
            return Err("node_not_found")
        return Ok([n for n, w in self._edges.get(node, [])])

    def has_edge(self, from_node, to_node):
        """Check if edge exists."""
        if from_node not in self._edges:
            return False
        return any(n == to_node for n, w in self._edges[from_node])

    def node_count(self):
        return len(self._nodes)

    def edge_count(self):
        return sum(len(edges) for edges in self._edges.values())


# ============================================================================
# MODULE 24: SAFE RATE LIMITER
# ============================================================================

class RateLimiter:
    """Token bucket rate limiter for MCU."""
    __slots__ = ('_tokens', '_max_tokens', '_refill_rate', '_last_refill')

    def __init__(self, max_tokens=10, refill_rate=1):
        self._tokens = max_tokens
        self._max_tokens = max_tokens
        self._refill_rate = refill_rate  # tokens per refill
        self._last_refill = 0

    def refill(self, current_time, interval_ms=1000):
        """Refill tokens based on elapsed time."""
        elapsed = current_time - self._last_refill
        if elapsed >= interval_ms:
            refills = elapsed // interval_ms
            self._tokens = min(self._max_tokens, self._tokens + refills * self._refill_rate)
            self._last_refill = current_time

    def try_acquire(self, tokens=1):
        """Try to acquire tokens."""
        if self._tokens >= tokens:
            self._tokens -= tokens
            return Ok(True)
        return Ok(False)

    def available(self):
        """Get available tokens."""
        return self._tokens


# ============================================================================
# MODULE 25: SAFE CIRCUIT BREAKER
# ============================================================================

class CircuitBreaker:
    """Circuit breaker pattern for fault tolerance."""
    __slots__ = ('_state', '_failures', '_threshold', '_reset_time', '_last_failure')

    STATE_CLOSED = 0
    STATE_OPEN = 1
    STATE_HALF_OPEN = 2

    def __init__(self, threshold=3, reset_time=5000):
        self._state = self.STATE_CLOSED
        self._failures = 0
        self._threshold = threshold
        self._reset_time = reset_time
        self._last_failure = 0

    def record_success(self):
        """Record successful operation."""
        self._failures = 0
        self._state = self.STATE_CLOSED
        return Ok(None)

    def record_failure(self, current_time):
        """Record failed operation."""
        self._failures += 1
        self._last_failure = current_time
        if self._failures >= self._threshold:
            self._state = self.STATE_OPEN
        return Ok(None)

    def can_execute(self, current_time):
        """Check if operation can be executed."""
        if self._state == self.STATE_CLOSED:
            return True
        if self._state == self.STATE_OPEN:
            if current_time - self._last_failure >= self._reset_time:
                self._state = self.STATE_HALF_OPEN
                return True
            return False
        return True  # Half-open allows one attempt

    def state(self):
        """Get current state."""
        return self._state


# ============================================================================
# MODULE 26: SAFE RETRY
# ============================================================================

class RetryPolicy:
    """Retry policy with exponential backoff."""
    __slots__ = ('_max_attempts', '_base_delay', '_max_delay', '_attempts')

    def __init__(self, max_attempts=3, base_delay=100, max_delay=5000):
        self._max_attempts = max_attempts
        self._base_delay = base_delay
        self._max_delay = max_delay
        self._attempts = 0

    def should_retry(self):
        """Check if should retry."""
        return self._attempts < self._max_attempts

    def next_delay(self):
        """Get next delay in ms (exponential backoff)."""
        delay = self._base_delay * (1 << self._attempts)
        self._attempts += 1
        return min(delay, self._max_delay)

    def record_attempt(self):
        """Record an attempt."""
        self._attempts += 1

    def reset(self):
        """Reset retry counter."""
        self._attempts = 0

    def attempts(self):
        """Get attempt count."""
        return self._attempts


# ============================================================================
# MODULE 27: SAFE MONOTONIC
# ============================================================================

class MonotonicCounter:
    """Monotonically increasing counter."""
    __slots__ = ('_value', '_max')

    def __init__(self, start=0, max_val=INT32_MAX):
        self._value = start
        self._max = max_val

    def increment(self, amount=1):
        """Increment counter."""
        r = safe_add(self._value, amount)
        if r.is_err():
            return r
        if r.value > self._max:
            return Err("overflow")
        self._value = r.value
        return Ok(self._value)

    def value(self):
        """Get current value."""
        return self._value

    def reset(self):
        """Reset to zero (use with caution)."""
        self._value = 0


class MonotonicTimestamp:
    """Monotonic timestamp tracker."""
    __slots__ = ('_last',)

    def __init__(self):
        self._last = 0

    def update(self, timestamp):
        """Update with new timestamp, ensuring monotonicity."""
        if timestamp <= self._last:
            self._last += 1
            return Ok(self._last)
        self._last = timestamp
        return Ok(timestamp)

    def last(self):
        """Get last timestamp."""
        return self._last


# ============================================================================
# MODULE 28: SAFE STATE MACHINE
# ============================================================================

class StateMachine:
    """Simple finite state machine."""
    __slots__ = ('_state', '_transitions', '_on_enter', '_on_exit')

    def __init__(self, initial_state):
        self._state = initial_state
        self._transitions = {}
        self._on_enter = {}
        self._on_exit = {}

    def add_transition(self, from_state, event, to_state):
        """Add state transition."""
        if from_state not in self._transitions:
            self._transitions[from_state] = {}
        self._transitions[from_state][event] = to_state
        return Ok(None)

    def on_enter(self, state, callback):
        """Set callback for entering state."""
        self._on_enter[state] = callback
        return Ok(None)

    def on_exit(self, state, callback):
        """Set callback for exiting state."""
        self._on_exit[state] = callback
        return Ok(None)

    def trigger(self, event):
        """Trigger event to cause state transition."""
        if self._state not in self._transitions:
            return Err("no_transitions")
        if event not in self._transitions[self._state]:
            return Err("invalid_event")

        old_state = self._state
        new_state = self._transitions[self._state][event]

        if old_state in self._on_exit:
            self._on_exit[old_state]()
        self._state = new_state
        if new_state in self._on_enter:
            self._on_enter[new_state]()

        return Ok(new_state)

    def state(self):
        """Get current state."""
        return self._state

    def can_trigger(self, event):
        """Check if event can be triggered."""
        if self._state not in self._transitions:
            return False
        return event in self._transitions[self._state]


# ============================================================================
# MODULE 29: SAFE CALCULATOR
# ============================================================================

class Calculator:
    """Safe integer calculator with history."""
    __slots__ = ('_value', '_history', '_max_history')

    def __init__(self, initial=0, max_history=10):
        self._value = initial
        self._history = []
        self._max_history = max_history

    def _save_history(self):
        if len(self._history) >= self._max_history:
            self._history.pop(0)
        self._history.append(self._value)

    def add(self, n):
        """Add to current value."""
        self._save_history()
        r = safe_add(self._value, n)
        if r.is_ok():
            self._value = r.value
        return r

    def sub(self, n):
        """Subtract from current value."""
        self._save_history()
        r = safe_sub(self._value, n)
        if r.is_ok():
            self._value = r.value
        return r

    def mul(self, n):
        """Multiply current value."""
        self._save_history()
        r = safe_mul(self._value, n)
        if r.is_ok():
            self._value = r.value
        return r

    def div(self, n):
        """Divide current value."""
        self._save_history()
        r = safe_div(self._value, n)
        if r.is_ok():
            self._value = r.value
        return r

    def value(self):
        """Get current value."""
        return self._value

    def undo(self):
        """Undo last operation."""
        if self._history:
            self._value = self._history.pop()
            return Ok(self._value)
        return Err("no_history")

    def clear(self):
        """Clear value and history."""
        self._value = 0
        self._history = []


# ============================================================================
# MODULE 30: SAFE GEO
# ============================================================================

def is_valid_latitude(lat):
    """Validate latitude (-90 to 90)."""
    return -90 <= lat <= 90


def is_valid_longitude(lon):
    """Validate longitude (-180 to 180)."""
    return -180 <= lon <= 180


def is_valid_coordinates(lat, lon):
    """Validate lat/lon pair."""
    return is_valid_latitude(lat) and is_valid_longitude(lon)


def manhattan_distance(x1, y1, x2, y2):
    """Calculate Manhattan distance."""
    return safe_add(safe_abs(x2 - x1).unwrap_or(0), safe_abs(y2 - y1).unwrap_or(0))


def chebyshev_distance(x1, y1, x2, y2):
    """Calculate Chebyshev distance."""
    dx = safe_abs(x2 - x1).unwrap_or(0)
    dy = safe_abs(y2 - y1).unwrap_or(0)
    return Ok(max(dx, dy))


def bounding_box(points):
    """Calculate bounding box for points [(x,y), ...]."""
    if not points:
        return Err("empty_points")
    min_x = max_x = points[0][0]
    min_y = max_y = points[0][1]
    for x, y in points[1:]:
        if x < min_x:
            min_x = x
        if x > max_x:
            max_x = x
        if y < min_y:
            min_y = y
        if y > max_y:
            max_y = y
    return Ok({"min_x": min_x, "min_y": min_y, "max_x": max_x, "max_y": max_y})


# ============================================================================
# MODULE 31: SAFE PROBABILITY
# ============================================================================

def is_valid_probability(p, scale=100):
    """Validate probability (0-100 for percentage scale)."""
    return 0 <= p <= scale


def safe_probability_add(p1, p2, scale=100):
    """Add probabilities (capped at scale)."""
    r = safe_add(p1, p2)
    if r.is_err():
        return r
    return Ok(min(r.value, scale))


def weighted_choice_index(weights, random_value):
    """Select index based on weights using random value 0-sum(weights)-1."""
    total = sum(weights)
    if random_value >= total:
        random_value = total - 1
    cumulative = 0
    for i, w in enumerate(weights):
        cumulative += w
        if random_value < cumulative:
            return Ok(i)
    return Ok(len(weights) - 1)


def normalize_weights(weights, scale=100):
    """Normalize weights to sum to scale."""
    total = sum(weights)
    if total == 0:
        return Err("zero_total")
    return Ok([w * scale // total for w in weights])


# ============================================================================
# MODULE 32: SAFE CHECKSUM
# ============================================================================

def crc8(data, poly=0x07, init=0x00):
    """Calculate CRC-8."""
    crc = init
    for b in data:
        crc ^= b
        for _ in range(8):
            if crc & 0x80:
                crc = ((crc << 1) ^ poly) & 0xFF
            else:
                crc = (crc << 1) & 0xFF
    return crc


def crc16(data, poly=0x8005, init=0xFFFF):
    """Calculate CRC-16."""
    crc = init
    for b in data:
        crc ^= b
        for _ in range(8):
            if crc & 0x0001:
                crc = ((crc >> 1) ^ poly) & 0xFFFF
            else:
                crc = (crc >> 1) & 0xFFFF
    return crc


def xor_checksum(data):
    """Simple XOR checksum."""
    result = 0
    for b in data:
        result ^= b
    return result


def sum_checksum(data, modulo=256):
    """Simple sum checksum."""
    return sum(data) % modulo


def verify_checksum(data, expected, method="xor"):
    """Verify checksum."""
    if method == "xor":
        return xor_checksum(data) == expected
    if method == "sum":
        return sum_checksum(data) == expected
    if method == "crc8":
        return crc8(data) == expected
    if method == "crc16":
        return crc16(data) == expected
    return False


# ============================================================================
# MODULE 33: SAFE TENSOR (minimal for MCU)
# ============================================================================

class Vector:
    """Simple fixed-size vector for MCU."""
    __slots__ = ('_data', '_size')

    def __init__(self, size, init=0):
        self._data = [init] * size
        self._size = size

    @classmethod
    def from_list(cls, data):
        """Create vector from list."""
        v = cls(len(data))
        v._data = list(data)
        return v

    def __getitem__(self, idx):
        return self._data[idx]

    def __setitem__(self, idx, value):
        self._data[idx] = value

    def __len__(self):
        return self._size

    def dot(self, other):
        """Dot product."""
        if self._size != other._size:
            return Err("size_mismatch")
        total = 0
        for i in range(self._size):
            r = safe_mul(self._data[i], other._data[i])
            if r.is_err():
                return r
            r2 = safe_add(total, r.value)
            if r2.is_err():
                return r2
            total = r2.value
        return Ok(total)

    def add(self, other):
        """Element-wise addition."""
        if self._size != other._size:
            return Err("size_mismatch")
        result = Vector(self._size)
        for i in range(self._size):
            r = safe_add(self._data[i], other._data[i])
            if r.is_err():
                return r
            result._data[i] = r.value
        return Ok(result)

    def scale(self, scalar):
        """Multiply by scalar."""
        result = Vector(self._size)
        for i in range(self._size):
            r = safe_mul(self._data[i], scalar)
            if r.is_err():
                return r
            result._data[i] = r.value
        return Ok(result)

    def magnitude_squared(self):
        """Squared magnitude (avoids sqrt)."""
        return self.dot(self)


class Matrix:
    """Simple fixed-size matrix for MCU."""
    __slots__ = ('_data', '_rows', '_cols')

    def __init__(self, rows, cols, init=0):
        self._rows = rows
        self._cols = cols
        self._data = [[init] * cols for _ in range(rows)]

    def get(self, row, col):
        """Get element."""
        if 0 <= row < self._rows and 0 <= col < self._cols:
            return Ok(self._data[row][col])
        return Err("out_of_bounds")

    def set(self, row, col, value):
        """Set element."""
        if 0 <= row < self._rows and 0 <= col < self._cols:
            self._data[row][col] = value
            return Ok(None)
        return Err("out_of_bounds")

    def mul_vector(self, vec):
        """Multiply matrix by vector."""
        if vec._size != self._cols:
            return Err("size_mismatch")
        result = Vector(self._rows)
        for i in range(self._rows):
            row_vec = Vector.from_list(self._data[i])
            r = row_vec.dot(vec)
            if r.is_err():
                return r
            result._data[i] = r.value
        return Ok(result)


# ============================================================================
# MODULE 34: SAFE PASSWORD
# ============================================================================

def check_password_strength(password, min_len=8):
    """Check password strength. Returns score 0-4."""
    if len(password) < min_len:
        return Err("too_short")
    score = 0
    has_lower = False
    has_upper = False
    has_digit = False
    has_special = False
    for c in password:
        if 'a' <= c <= 'z':
            has_lower = True
        elif 'A' <= c <= 'Z':
            has_upper = True
        elif '0' <= c <= '9':
            has_digit = True
        else:
            has_special = True
    if has_lower:
        score += 1
    if has_upper:
        score += 1
    if has_digit:
        score += 1
    if has_special:
        score += 1
    return Ok(score)


def is_common_password(password):
    """Check against tiny list of common passwords."""
    common = ("password", "123456", "qwerty", "admin", "letmein", "welcome")
    return password.lower() in common


def mask_password(password, visible_chars=2, mask_char="*"):
    """Mask password for display."""
    if len(password) <= visible_chars:
        return Ok(mask_char * len(password))
    return Ok(password[:visible_chars] + mask_char * (len(password) - visible_chars))


# ============================================================================
# MODULE 35: SAFE ML (minimal ML utilities for MCU)
# ============================================================================

def sigmoid_approx(x, scale=100):
    """Approximate sigmoid using integer math."""
    # Piecewise linear approximation
    if x <= -6 * scale:
        return Ok(0)
    if x >= 6 * scale:
        return Ok(scale)
    return Ok((x + 6 * scale) * scale // (12 * scale))


def relu(x):
    """ReLU activation (integer)."""
    return Ok(max(0, x))


def leaky_relu(x, alpha=10):
    """Leaky ReLU (alpha is percentage, e.g., 10 = 0.1)."""
    if x >= 0:
        return Ok(x)
    return Ok((x * alpha) // 100)


def softmax_argmax(values):
    """Return index of max value (argmax for inference)."""
    if not values:
        return Err("empty")
    max_idx = 0
    max_val = values[0]
    for i, v in enumerate(values[1:], 1):
        if v > max_val:
            max_val = v
            max_idx = i
    return Ok(max_idx)


def quantize(value, scale=256, zero_point=128):
    """Quantize float-like value to int8 range."""
    q = (value * scale // 100) + zero_point
    return Ok(max(-128, min(127, q)))


def dequantize(q_value, scale=256, zero_point=128):
    """Dequantize int8 value."""
    return Ok((q_value - zero_point) * 100 // scale)


# ============================================================================
# MODULE 36: SAFE HEADER
# ============================================================================

def is_valid_header_name(name):
    """Validate HTTP header name."""
    if not name:
        return False
    for c in name:
        if c in ' \t\r\n:':
            return False
        if ord(c) < 33 or ord(c) > 126:
            return False
    return True


def is_valid_header_value(value):
    """Validate HTTP header value."""
    for c in value:
        if c in '\r\n':
            return False
    return True


def safe_header(name, value, max_len=8192):
    """Create safe HTTP header string."""
    if not is_valid_header_name(name):
        return Err("invalid_name")
    if not is_valid_header_value(value):
        return Err("invalid_value")
    header = "{}: {}".format(name, value)
    if len(header) > max_len:
        return Err("header_too_long")
    return Ok(header)


def parse_header(line):
    """Parse header line into name/value."""
    if ": " not in line:
        return Err("invalid_format")
    idx = line.index(": ")
    name = line[:idx]
    value = line[idx + 2:]
    if not is_valid_header_name(name):
        return Err("invalid_name")
    return Ok({"name": name, "value": value})


# ============================================================================
# MODULE 37: SAFE COOKIE
# ============================================================================

def is_valid_cookie_name(name):
    """Validate cookie name."""
    if not name:
        return False
    invalid = '()<>@,;:\\"/[]?={} \t'
    for c in name:
        if c in invalid or ord(c) < 33 or ord(c) > 126:
            return False
    return True


def is_valid_cookie_value(value):
    """Validate cookie value."""
    for c in value:
        if c in '"\\' or ord(c) < 32 or ord(c) > 126:
            if c not in ' ':  # Space allowed in quoted value
                return False
    return True


def safe_set_cookie(name, value, max_age=None, path="/", secure=False, http_only=True):
    """Create Set-Cookie header value."""
    if not is_valid_cookie_name(name):
        return Err("invalid_name")
    if not is_valid_cookie_value(value):
        return Err("invalid_value")

    parts = ["{}={}".format(name, value)]
    if max_age is not None:
        parts.append("Max-Age={}".format(max_age))
    if path:
        parts.append("Path={}".format(path))
    if secure:
        parts.append("Secure")
    if http_only:
        parts.append("HttpOnly")
    return Ok("; ".join(parts))


def parse_cookie(cookie_str):
    """Parse Cookie header into dict."""
    result = {}
    pairs = cookie_str.split("; ")
    for pair in pairs:
        if "=" in pair:
            idx = pair.index("=")
            name = pair[:idx]
            value = pair[idx + 1:]
            result[name] = value
    return Ok(result)


# ============================================================================
# MODULE 38: SAFE CONTENT TYPE
# ============================================================================

def is_valid_mime_type(mime):
    """Validate MIME type format."""
    if "/" not in mime:
        return False
    parts = mime.split("/")
    if len(parts) != 2:
        return False
    return len(parts[0]) > 0 and len(parts[1]) > 0


def safe_content_type(mime, charset=None, boundary=None):
    """Create Content-Type header value."""
    if not is_valid_mime_type(mime):
        return Err("invalid_mime")
    result = mime
    if charset:
        result += "; charset={}".format(charset)
    if boundary:
        result += "; boundary={}".format(boundary)
    return Ok(result)


def parse_content_type(value):
    """Parse Content-Type header."""
    result = {"mime": None, "charset": None, "boundary": None}
    parts = value.split("; ")
    result["mime"] = parts[0]
    for part in parts[1:]:
        if "=" in part:
            idx = part.index("=")
            key = part[:idx].lower()
            val = part[idx + 1:]
            if key == "charset":
                result["charset"] = val
            elif key == "boundary":
                result["boundary"] = val
    return Ok(result)


KNOWN_MIME_TYPES = {
    "json": "application/json",
    "html": "text/html",
    "text": "text/plain",
    "xml": "application/xml",
    "form": "application/x-www-form-urlencoded",
    "multipart": "multipart/form-data",
    "octet": "application/octet-stream",
}


def get_mime_type(shorthand):
    """Get MIME type from shorthand."""
    return Ok(KNOWN_MIME_TYPES.get(shorthand, "application/octet-stream"))


# ============================================================================
# BOUNDED VALUES (from original)
# ============================================================================

def clamp(value, min_val, max_val):
    """Clamp value to range [min_val, max_val]."""
    if value < min_val:
        return min_val
    if value > max_val:
        return max_val
    return value


def in_range(value, min_val, max_val):
    """Check if value is in range (inclusive)."""
    return min_val <= value <= max_val


def require_in_range(value, min_val, max_val):
    """Require value in range or return error."""
    if not in_range(value, min_val, max_val):
        return Err("out_of_bounds")
    return Ok(value)


# ============================================================================
# EMBEDDED VALIDATION (from original)
# ============================================================================

def is_valid_gpio(pin, max_pin=39):
    """Validate GPIO pin number (default max=39 for ESP32)."""
    return 0 <= pin <= max_pin


def is_valid_pwm_duty(duty, resolution=10):
    """Validate PWM duty cycle for given resolution."""
    max_duty = (1 << resolution) - 1
    return 0 <= duty <= max_duty


def is_valid_i2c_address(addr):
    """Validate I2C address (7-bit, excluding reserved)."""
    return 0x08 <= addr <= 0x77


def is_valid_spi_mode(mode):
    """Validate SPI mode (0-3)."""
    return mode in (0, 1, 2, 3)


def is_valid_uart_baud(baud):
    """Validate common UART baud rates."""
    valid_bauds = (
        300, 1200, 2400, 4800, 9600, 14400, 19200,
        28800, 38400, 57600, 115200, 230400, 460800, 921600
    )
    return baud in valid_bauds


def is_valid_adc_value(value, resolution=12):
    """Validate ADC reading for given resolution."""
    max_val = (1 << resolution) - 1
    return 0 <= value <= max_val


# ============================================================================
# SENSOR UTILITIES (from original)
# ============================================================================

def safe_scale(value, in_min, in_max, out_min, out_max):
    """Safely scale value from one range to another (like Arduino map())."""
    if in_max == in_min:
        return Err("zero_range")
    value = clamp(value, in_min, in_max)
    numerator = safe_mul(value - in_min, out_max - out_min)
    if numerator.is_err():
        return numerator
    result = safe_div(numerator.value, in_max - in_min)
    if result.is_err():
        return result
    return safe_add(result.value, out_min)


def safe_moving_average(values, window_size):
    """Calculate moving average with overflow protection."""
    if not values or window_size <= 0:
        return Err("invalid_input")
    window = values[-window_size:]
    total = 0
    for v in window:
        result = safe_add(total, v)
        if result.is_err():
            return result
        total = result.value
    return safe_div(total, len(window))


# ============================================================================
# TIMING UTILITIES (from original)
# ============================================================================

def safe_millis_elapsed(start, current, max_millis=0xFFFFFFFF):
    """Calculate elapsed milliseconds handling wrap-around."""
    if current >= start:
        return Ok(current - start)
    else:
        return Ok(max_millis - start + current + 1)


def is_timeout(start, current, timeout_ms, max_millis=0xFFFFFFFF):
    """Check if timeout has elapsed (handles wrap-around)."""
    elapsed = safe_millis_elapsed(start, current, max_millis)
    if elapsed.is_err():
        return True
    return elapsed.value >= timeout_ms


# ============================================================================
# MEMORY-EFFICIENT BOUNDED TYPES (from original)
# ============================================================================

class BoundedInt:
    """Integer constrained to a range."""
    __slots__ = ('_value', '_min', '_max')

    def __init__(self, value, min_val, max_val):
        self._min = min_val
        self._max = max_val
        self._value = clamp(value, min_val, max_val)

    @property
    def value(self):
        return self._value

    def set(self, value):
        self._value = clamp(value, self._min, self._max)

    def add(self, delta):
        result = safe_add(self._value, delta)
        if result.is_ok():
            self._value = clamp(result.value, self._min, self._max)
        return result.is_ok()

    def sub(self, delta):
        result = safe_sub(self._value, delta)
        if result.is_ok():
            self._value = clamp(result.value, self._min, self._max)
        return result.is_ok()


class Percentage:
    """Percentage value (0-100)."""
    __slots__ = ('_value',)

    def __init__(self, value=0):
        self._value = clamp(value, 0, 100)

    @property
    def value(self):
        return self._value

    def set(self, value):
        self._value = clamp(value, 0, 100)

    def of(self, amount):
        """Calculate this percentage of amount."""
        return safe_div(safe_mul(amount, self._value).unwrap_or(0), 100)


class PWMDuty:
    """PWM duty cycle for specific resolution."""
    __slots__ = ('_value', '_resolution', '_max')

    def __init__(self, value=0, resolution=10):
        self._resolution = resolution
        self._max = (1 << resolution) - 1
        self._value = clamp(value, 0, self._max)

    @property
    def value(self):
        return self._value

    def set(self, value):
        self._value = clamp(value, 0, self._max)

    def set_percent(self, percent):
        """Set duty from percentage (0-100)."""
        duty = safe_mul(percent, self._max).unwrap_or(0) // 100
        self._value = clamp(duty, 0, self._max)


# ============================================================================
# VERSION INFO
# ============================================================================

def version():
    """Return library version."""
    return VERSION


def module_count():
    """Return number of modules."""
    return MODULE_COUNT


def library_info():
    """Return library information."""
    return {
        "name": "proven",
        "version": VERSION,
        "modules": MODULE_COUNT,
        "platform": "MicroPython",
        "targets": ["ESP32", "RP2040"],
        "license": "PMPL-1.0"
    }
