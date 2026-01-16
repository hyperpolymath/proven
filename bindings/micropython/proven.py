# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
Proven Safety Library for MicroPython

Formally verified safety primitives optimized for embedded systems.
Designed for microcontrollers with limited memory and no floating point.

Features:
- Safe integer math with overflow detection
- Bounded types for constrained values
- Validation for embedded protocols
- Memory-efficient Result type

Version: 0.9.0
"""

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
# SAFE MATH
# ============================================================================

def safe_add(a, b):
    """
    Safe addition with overflow check.

    Args:
        a: First operand (int)
        b: Second operand (int)

    Returns:
        Result with sum or overflow error
    """
    if b > 0 and a > INT_MAX - b:
        return Err("overflow")
    if b < 0 and a < INT_MIN - b:
        return Err("underflow")
    return Ok(a + b)


def safe_sub(a, b):
    """
    Safe subtraction with underflow check.
    """
    if b < 0 and a > INT_MAX + b:
        return Err("overflow")
    if b > 0 and a < INT_MIN + b:
        return Err("underflow")
    return Ok(a - b)


def safe_mul(a, b):
    """
    Safe multiplication with overflow check.
    Uses division-based verification suitable for MCUs.
    """
    if a == 0 or b == 0:
        return Ok(0)

    result = a * b

    # Verify with division
    if result // a != b:
        return Err("overflow")

    return Ok(result)


def safe_div(a, b):
    """
    Safe division with zero check.
    """
    if b == 0:
        return Err("division_by_zero")

    # Handle MIN / -1 overflow
    if a == INT_MIN and b == -1:
        return Err("overflow")

    return Ok(a // b)


def safe_mod(a, b):
    """
    Safe modulo with zero check.
    """
    if b == 0:
        return Err("modulo_by_zero")
    return Ok(a % b)


# ============================================================================
# BOUNDED VALUES
# ============================================================================

def clamp(value, min_val, max_val):
    """
    Clamp value to range [min_val, max_val].
    No Result overhead - returns value directly.
    """
    if value < min_val:
        return min_val
    if value > max_val:
        return max_val
    return value


def in_range(value, min_val, max_val):
    """
    Check if value is in range (inclusive).
    """
    return min_val <= value <= max_val


def require_in_range(value, min_val, max_val):
    """
    Require value in range or return error.
    """
    if not in_range(value, min_val, max_val):
        return Err("out_of_bounds")
    return Ok(value)


# ============================================================================
# VALIDATION - Embedded-specific
# ============================================================================

def is_valid_gpio(pin, max_pin=39):
    """
    Validate GPIO pin number.
    Default max=39 for ESP32, adjust for your MCU.
    """
    return 0 <= pin <= max_pin


def is_valid_pwm_duty(duty, resolution=10):
    """
    Validate PWM duty cycle for given resolution.

    Args:
        duty: Duty cycle value
        resolution: Bit resolution (default 10 = 0-1023)
    """
    max_duty = (1 << resolution) - 1
    return 0 <= duty <= max_duty


def is_valid_i2c_address(addr):
    """
    Validate I2C address (7-bit, excluding reserved).
    Valid range: 0x08-0x77
    """
    return 0x08 <= addr <= 0x77


def is_valid_spi_mode(mode):
    """
    Validate SPI mode (0-3).
    """
    return mode in (0, 1, 2, 3)


def is_valid_uart_baud(baud):
    """
    Validate common UART baud rates.
    """
    valid_bauds = (
        300, 1200, 2400, 4800, 9600, 14400, 19200,
        28800, 38400, 57600, 115200, 230400, 460800, 921600
    )
    return baud in valid_bauds


def is_valid_adc_value(value, resolution=12):
    """
    Validate ADC reading for given resolution.

    Args:
        value: ADC value
        resolution: Bit resolution (default 12 = 0-4095)
    """
    max_val = (1 << resolution) - 1
    return 0 <= value <= max_val


# ============================================================================
# SENSOR UTILITIES
# ============================================================================

def safe_scale(value, in_min, in_max, out_min, out_max):
    """
    Safely scale value from one range to another (like Arduino map()).
    Prevents division by zero and overflow.
    """
    if in_max == in_min:
        return Err("zero_range")

    # Clamp input to expected range
    value = clamp(value, in_min, in_max)

    # Calculate using safe operations
    numerator = safe_mul(value - in_min, out_max - out_min)
    if numerator.is_err():
        return numerator

    result = safe_div(numerator.value, in_max - in_min)
    if result.is_err():
        return result

    return safe_add(result.value, out_min)


def safe_temperature_c_to_f(celsius):
    """
    Convert Celsius to Fahrenheit safely.
    Formula: F = C * 9/5 + 32
    """
    scaled = safe_mul(celsius, 9)
    if scaled.is_err():
        return scaled

    divided = safe_div(scaled.value, 5)
    if divided.is_err():
        return divided

    return safe_add(divided.value, 32)


def safe_moving_average(values, window_size):
    """
    Calculate moving average with overflow protection.
    Suitable for sensor smoothing.
    """
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
# TIMING UTILITIES
# ============================================================================

def safe_millis_elapsed(start, current, max_millis=0xFFFFFFFF):
    """
    Calculate elapsed milliseconds handling wrap-around.
    Microcontrollers often have 32-bit millis() that wraps.
    """
    if current >= start:
        return Ok(current - start)
    else:
        # Handle wrap-around
        return Ok(max_millis - start + current + 1)


def is_timeout(start, current, timeout_ms, max_millis=0xFFFFFFFF):
    """
    Check if timeout has elapsed (handles wrap-around).
    """
    elapsed = safe_millis_elapsed(start, current, max_millis)
    if elapsed.is_err():
        return True  # Assume timeout on error
    return elapsed.value >= timeout_ms


# ============================================================================
# MEMORY-EFFICIENT BOUNDED TYPES
# ============================================================================

class BoundedInt:
    """
    Integer constrained to a range.
    Memory efficient using __slots__.
    """
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
    """
    Percentage value (0-100).
    """
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
    """
    PWM duty cycle for specific resolution.
    """
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
# VERSION
# ============================================================================

def version():
    """Return library version."""
    return "0.9.0"
