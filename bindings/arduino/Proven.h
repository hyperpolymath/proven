// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file Proven.h
 * @brief Proven Safety Library for Arduino
 *
 * Formally verified safety primitives optimized for embedded systems.
 * Provides safe math, bounded types, and validation for Arduino projects.
 *
 * @version 0.9.0
 */

#ifndef PROVEN_H
#define PROVEN_H

#include <Arduino.h>
#include <limits.h>

// ============================================================================
// CONFIGURATION
// ============================================================================

// Use 32-bit integers on most Arduinos, 16-bit on AVR if needed
#if defined(__AVR__)
  #define PROVEN_INT_MAX LONG_MAX
  #define PROVEN_INT_MIN LONG_MIN
  typedef long proven_int_t;
#else
  #define PROVEN_INT_MAX INT32_MAX
  #define PROVEN_INT_MIN INT32_MIN
  typedef int32_t proven_int_t;
#endif

// Library version
#define PROVEN_VERSION "0.9.0"

// ============================================================================
// RESULT TYPE
// ============================================================================

namespace proven {

/**
 * @brief Result type for safe operations
 * @tparam T Value type
 */
template<typename T>
class Result {
public:
    Result(bool ok, T value, const char* error = nullptr)
        : _ok(ok), _value(value), _error(error) {}

    inline bool isOk() const { return _ok; }
    inline bool isErr() const { return !_ok; }

    /**
     * @brief Get value, returns default if error
     */
    inline T unwrapOr(T defaultValue) const {
        return _ok ? _value : defaultValue;
    }

    /**
     * @brief Get value (check isOk() first!)
     */
    inline T value() const { return _value; }

    /**
     * @brief Get error message
     */
    inline const char* error() const { return _error; }

private:
    bool _ok;
    T _value;
    const char* _error;
};

/**
 * @brief Create success result
 */
template<typename T>
inline Result<T> Ok(T value) {
    return Result<T>(true, value);
}

/**
 * @brief Create error result
 */
template<typename T>
inline Result<T> Err(const char* error, T defaultValue = T()) {
    return Result<T>(false, defaultValue, error);
}

// ============================================================================
// SAFE MATH
// ============================================================================

/**
 * @brief Safe addition with overflow check
 */
inline Result<proven_int_t> safeAdd(proven_int_t a, proven_int_t b) {
    if (b > 0 && a > PROVEN_INT_MAX - b) {
        return Err<proven_int_t>("overflow");
    }
    if (b < 0 && a < PROVEN_INT_MIN - b) {
        return Err<proven_int_t>("underflow");
    }
    return Ok<proven_int_t>(a + b);
}

/**
 * @brief Safe subtraction with underflow check
 */
inline Result<proven_int_t> safeSub(proven_int_t a, proven_int_t b) {
    if (b < 0 && a > PROVEN_INT_MAX + b) {
        return Err<proven_int_t>("overflow");
    }
    if (b > 0 && a < PROVEN_INT_MIN + b) {
        return Err<proven_int_t>("underflow");
    }
    return Ok<proven_int_t>(a - b);
}

/**
 * @brief Safe multiplication with overflow check
 */
inline Result<proven_int_t> safeMul(proven_int_t a, proven_int_t b) {
    if (a == 0 || b == 0) {
        return Ok<proven_int_t>(0);
    }

    proven_int_t result = a * b;

    // Verify with division
    if (result / a != b) {
        return Err<proven_int_t>("overflow");
    }

    return Ok<proven_int_t>(result);
}

/**
 * @brief Safe division with zero check
 */
inline Result<proven_int_t> safeDiv(proven_int_t a, proven_int_t b) {
    if (b == 0) {
        return Err<proven_int_t>("division_by_zero");
    }

    // Handle INT_MIN / -1 overflow
    if (a == PROVEN_INT_MIN && b == -1) {
        return Err<proven_int_t>("overflow");
    }

    return Ok<proven_int_t>(a / b);
}

/**
 * @brief Safe modulo with zero check
 */
inline Result<proven_int_t> safeMod(proven_int_t a, proven_int_t b) {
    if (b == 0) {
        return Err<proven_int_t>("modulo_by_zero");
    }
    return Ok<proven_int_t>(a % b);
}

// ============================================================================
// BOUNDED VALUES
// ============================================================================

/**
 * @brief Clamp value to range [minVal, maxVal]
 */
template<typename T>
inline T clamp(T value, T minVal, T maxVal) {
    if (value < minVal) return minVal;
    if (value > maxVal) return maxVal;
    return value;
}

/**
 * @brief Check if value is in range (inclusive)
 */
template<typename T>
inline bool inRange(T value, T minVal, T maxVal) {
    return value >= minVal && value <= maxVal;
}

/**
 * @brief Safe map() function with overflow protection
 * @note Replacement for Arduino's map() that can overflow
 */
inline Result<proven_int_t> safeMap(
    proven_int_t x,
    proven_int_t inMin, proven_int_t inMax,
    proven_int_t outMin, proven_int_t outMax
) {
    if (inMax == inMin) {
        return Err<proven_int_t>("zero_range");
    }

    x = clamp(x, inMin, inMax);

    auto diff = safeSub(x, inMin);
    if (diff.isErr()) return diff;

    auto outRange = safeSub(outMax, outMin);
    if (outRange.isErr()) return outRange;

    auto scaled = safeMul(diff.value(), outRange.value());
    if (scaled.isErr()) return scaled;

    auto inRange = safeSub(inMax, inMin);
    if (inRange.isErr()) return inRange;

    auto divided = safeDiv(scaled.value(), inRange.value());
    if (divided.isErr()) return divided;

    return safeAdd(divided.value(), outMin);
}

// ============================================================================
// VALIDATION - Arduino-specific
// ============================================================================

/**
 * @brief Validate GPIO pin number
 * @param pin Pin number
 * @param maxPin Maximum valid pin (varies by board)
 */
inline bool isValidGpio(uint8_t pin, uint8_t maxPin = NUM_DIGITAL_PINS - 1) {
    return pin <= maxPin;
}

/**
 * @brief Validate analog pin
 */
inline bool isValidAnalogPin(uint8_t pin) {
    return pin < NUM_ANALOG_INPUTS;
}

/**
 * @brief Validate PWM duty cycle
 * @param duty Duty value
 * @param resolution Bit resolution (8 for analogWrite on AVR)
 */
inline bool isValidPwmDuty(uint16_t duty, uint8_t resolution = 8) {
    uint16_t maxDuty = (1 << resolution) - 1;
    return duty <= maxDuty;
}

/**
 * @brief Validate I2C address (7-bit, excluding reserved)
 */
inline bool isValidI2cAddress(uint8_t addr) {
    return addr >= 0x08 && addr <= 0x77;
}

/**
 * @brief Validate SPI mode (0-3)
 */
inline bool isValidSpiMode(uint8_t mode) {
    return mode <= 3;
}

/**
 * @brief Validate common baud rate
 */
inline bool isValidBaudRate(unsigned long baud) {
    switch (baud) {
        case 300:
        case 1200:
        case 2400:
        case 4800:
        case 9600:
        case 14400:
        case 19200:
        case 28800:
        case 38400:
        case 57600:
        case 115200:
        case 230400:
        case 460800:
        case 921600:
            return true;
        default:
            return false;
    }
}

/**
 * @brief Validate ADC value for resolution
 * @param value ADC reading
 * @param resolution Bit resolution (10 for AVR, 12 for many ARM)
 */
inline bool isValidAdcValue(uint16_t value, uint8_t resolution = 10) {
    uint16_t maxVal = (1 << resolution) - 1;
    return value <= maxVal;
}

// ============================================================================
// TIMING UTILITIES
// ============================================================================

/**
 * @brief Calculate elapsed milliseconds handling wrap-around
 */
inline unsigned long millisElapsed(unsigned long start, unsigned long current) {
    if (current >= start) {
        return current - start;
    } else {
        // Handle wrap-around (unsigned long wraps at ~49 days)
        return ULONG_MAX - start + current + 1;
    }
}

/**
 * @brief Check if timeout has elapsed (handles wrap-around)
 */
inline bool isTimeout(unsigned long start, unsigned long current, unsigned long timeoutMs) {
    return millisElapsed(start, current) >= timeoutMs;
}

/**
 * @brief Check if timeout has elapsed using millis()
 */
inline bool isTimeout(unsigned long start, unsigned long timeoutMs) {
    return isTimeout(start, millis(), timeoutMs);
}

// ============================================================================
// BOUNDED TYPES
// ============================================================================

/**
 * @brief Integer bounded to a range
 * @tparam T Underlying integer type
 */
template<typename T = int16_t>
class BoundedInt {
public:
    BoundedInt(T value, T minVal, T maxVal)
        : _min(minVal), _max(maxVal), _value(clamp(value, minVal, maxVal)) {}

    inline T value() const { return _value; }

    void set(T value) {
        _value = clamp(value, _min, _max);
    }

    bool add(T delta) {
        auto result = safeAdd(_value, delta);
        if (result.isOk()) {
            _value = clamp(result.value(), _min, _max);
            return true;
        }
        return false;
    }

    bool sub(T delta) {
        auto result = safeSub(_value, delta);
        if (result.isOk()) {
            _value = clamp(result.value(), _min, _max);
            return true;
        }
        return false;
    }

    inline T min() const { return _min; }
    inline T max() const { return _max; }

private:
    T _min;
    T _max;
    T _value;
};

/**
 * @brief Percentage value (0-100)
 */
class Percentage {
public:
    Percentage(uint8_t value = 0) : _value(clamp<uint8_t>(value, 0, 100)) {}

    inline uint8_t value() const { return _value; }

    void set(uint8_t value) {
        _value = clamp<uint8_t>(value, 0, 100);
    }

    /**
     * @brief Calculate this percentage of amount
     */
    inline proven_int_t of(proven_int_t amount) {
        return (amount * _value) / 100;
    }

    /**
     * @brief Convert to PWM duty cycle
     * @param resolution PWM resolution in bits
     */
    inline uint16_t toPwmDuty(uint8_t resolution = 8) {
        uint16_t maxDuty = (1 << resolution) - 1;
        return ((uint32_t)_value * maxDuty) / 100;
    }

private:
    uint8_t _value;
};

/**
 * @brief PWM duty cycle with resolution
 */
class PwmDuty {
public:
    PwmDuty(uint16_t value = 0, uint8_t resolution = 8)
        : _resolution(resolution),
          _max((1 << resolution) - 1),
          _value(clamp(value, (uint16_t)0, _max)) {}

    inline uint16_t value() const { return _value; }
    inline uint16_t max() const { return _max; }

    void set(uint16_t value) {
        _value = clamp(value, (uint16_t)0, _max);
    }

    /**
     * @brief Set from percentage (0-100)
     */
    void setPercent(uint8_t percent) {
        percent = clamp<uint8_t>(percent, 0, 100);
        _value = ((uint32_t)percent * _max) / 100;
    }

    /**
     * @brief Get as percentage
     */
    inline uint8_t toPercent() const {
        return ((uint32_t)_value * 100) / _max;
    }

private:
    uint8_t _resolution;
    uint16_t _max;
    uint16_t _value;
};

// ============================================================================
// SENSOR UTILITIES
// ============================================================================

/**
 * @brief Convert Celsius to Fahrenheit safely
 */
inline Result<proven_int_t> celsiusToFahrenheit(proven_int_t celsius) {
    auto scaled = safeMul(celsius, 9);
    if (scaled.isErr()) return scaled;

    auto divided = safeDiv(scaled.value(), 5);
    if (divided.isErr()) return divided;

    return safeAdd(divided.value(), 32);
}

/**
 * @brief Convert Fahrenheit to Celsius safely
 */
inline Result<proven_int_t> fahrenheitToCelsius(proven_int_t fahrenheit) {
    auto offset = safeSub(fahrenheit, 32);
    if (offset.isErr()) return offset;

    auto scaled = safeMul(offset.value(), 5);
    if (scaled.isErr()) return scaled;

    return safeDiv(scaled.value(), 9);
}

/**
 * @brief Simple exponential moving average filter
 * @param current Current reading
 * @param previous Previous filtered value
 * @param alpha Smoothing factor (0-255, higher = more weight to current)
 */
inline int16_t emaFilter(int16_t current, int16_t previous, uint8_t alpha = 64) {
    // EMA = alpha * current + (1 - alpha) * previous
    // Using fixed-point: result = (alpha * current + (256 - alpha) * previous) / 256
    int32_t result = (int32_t)alpha * current + (int32_t)(256 - alpha) * previous;
    return (int16_t)(result / 256);
}

// ============================================================================
// VERSION
// ============================================================================

/**
 * @brief Get library version string
 */
inline const char* version() {
    return PROVEN_VERSION;
}

} // namespace proven

#endif // PROVEN_H
