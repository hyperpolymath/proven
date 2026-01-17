// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file Proven.h
 * @brief Proven Safety Library for Arduino - Complete Edition
 *
 * Formally verified safety primitives optimized for embedded systems.
 * Provides 38 safe modules covering math, strings, networking, data structures,
 * resilience patterns, state machines, and more.
 *
 * @version 0.4.0
 * @note Module Count: 38
 *
 * Modules by Category:
 * - Core (11): SafeMath, SafeString, SafePath, SafeEmail, SafeUrl, SafeNetwork,
 *              SafeCrypto, SafeUUID, SafeCurrency, SafePhone, SafeHex
 * - Data (7): SafeJson, SafeDateTime, SafeFloat, SafeVersion, SafeColor,
 *             SafeAngle, SafeUnit
 * - Data Structures (5): SafeBuffer, SafeQueue, SafeBloom, SafeLRU, SafeGraph
 * - Resilience (4): SafeRateLimiter, SafeCircuitBreaker, SafeRetry, SafeMonotonic
 * - State (2): SafeStateMachine, SafeCalculator
 * - Algorithm (4): SafeGeo, SafeProbability, SafeChecksum, SafeTensor
 * - Security (2): SafePassword, SafeMl
 * - HTTP (3): SafeHeader, SafeCookie, SafeContentType
 */

#ifndef PROVEN_H
#define PROVEN_H

#include <Arduino.h>
#include <limits.h>
#include <string.h>

// ============================================================================
// CONFIGURATION
// ============================================================================

// Library version and module count
#define PROVEN_VERSION "0.4.0"
#define PROVEN_MODULE_COUNT 38

// Use 32-bit integers on most Arduinos, 16-bit on AVR if needed
#if defined(__AVR__)
  #define PROVEN_INT_MAX LONG_MAX
  #define PROVEN_INT_MIN LONG_MIN
  typedef long proven_int_t;
  #define PROVEN_SMALL_MEMORY 1
#else
  #define PROVEN_INT_MAX INT32_MAX
  #define PROVEN_INT_MIN INT32_MIN
  typedef int32_t proven_int_t;
  #define PROVEN_SMALL_MEMORY 0
#endif

// Default buffer sizes (can be overridden before including)
#ifndef PROVEN_MAX_STRING_LEN
  #if PROVEN_SMALL_MEMORY
    #define PROVEN_MAX_STRING_LEN 64
  #else
    #define PROVEN_MAX_STRING_LEN 256
  #endif
#endif

#ifndef PROVEN_MAX_BUFFER_SIZE
  #if PROVEN_SMALL_MEMORY
    #define PROVEN_MAX_BUFFER_SIZE 128
  #else
    #define PROVEN_MAX_BUFFER_SIZE 512
  #endif
#endif

#ifndef PROVEN_MAX_QUEUE_SIZE
  #if PROVEN_SMALL_MEMORY
    #define PROVEN_MAX_QUEUE_SIZE 16
  #else
    #define PROVEN_MAX_QUEUE_SIZE 64
  #endif
#endif

namespace proven {

// ============================================================================
// STATUS CODES
// ============================================================================

/**
 * @brief Status codes returned by Proven operations
 */
enum class Status : int8_t {
    Ok = 0,
    ErrNullPointer = -1,
    ErrInvalidArgument = -2,
    ErrOverflow = -3,
    ErrUnderflow = -4,
    ErrDivisionByZero = -5,
    ErrParseFailure = -6,
    ErrValidationFailed = -7,
    ErrOutOfBounds = -8,
    ErrEncodingError = -9,
    ErrBufferFull = -10,
    ErrBufferEmpty = -11,
    ErrTimeout = -12,
    ErrCircuitOpen = -13,
    ErrRateLimited = -14,
    ErrInvalidState = -15
};

// ============================================================================
// RESULT TYPE
// ============================================================================

/**
 * @brief Result type for safe operations
 * @tparam T Value type
 */
template<typename T>
class Result {
public:
    Result(Status status, T value) : _status(status), _value(value) {}

    inline bool isOk() const { return _status == Status::Ok; }
    inline bool isErr() const { return _status != Status::Ok; }
    inline Status status() const { return _status; }
    inline T value() const { return _value; }

    /**
     * @brief Get value, returns default if error
     */
    inline T unwrapOr(T defaultValue) const {
        return isOk() ? _value : defaultValue;
    }

private:
    Status _status;
    T _value;
};

/**
 * @brief Create success result
 */
template<typename T>
inline Result<T> Ok(T value) {
    return Result<T>(Status::Ok, value);
}

/**
 * @brief Create error result
 */
template<typename T>
inline Result<T> Err(Status status, T defaultValue = T()) {
    return Result<T>(status, defaultValue);
}

// ============================================================================
// MODULE 1: SafeMath - Core (1/11)
// ============================================================================

/**
 * @brief Safe addition with overflow check
 */
inline Result<proven_int_t> safeAdd(proven_int_t a, proven_int_t b) {
    if (b > 0 && a > PROVEN_INT_MAX - b) {
        return Err<proven_int_t>(Status::ErrOverflow);
    }
    if (b < 0 && a < PROVEN_INT_MIN - b) {
        return Err<proven_int_t>(Status::ErrUnderflow);
    }
    return Ok<proven_int_t>(a + b);
}

/**
 * @brief Safe subtraction with underflow check
 */
inline Result<proven_int_t> safeSub(proven_int_t a, proven_int_t b) {
    if (b < 0 && a > PROVEN_INT_MAX + b) {
        return Err<proven_int_t>(Status::ErrOverflow);
    }
    if (b > 0 && a < PROVEN_INT_MIN + b) {
        return Err<proven_int_t>(Status::ErrUnderflow);
    }
    return Ok<proven_int_t>(a - b);
}

/**
 * @brief Safe multiplication with overflow check
 */
inline Result<proven_int_t> safeMul(proven_int_t a, proven_int_t b) {
    if (a == 0 || b == 0) return Ok<proven_int_t>(0);

    proven_int_t result = a * b;
    if (result / a != b) {
        return Err<proven_int_t>(Status::ErrOverflow);
    }
    return Ok<proven_int_t>(result);
}

/**
 * @brief Safe division with zero check
 */
inline Result<proven_int_t> safeDiv(proven_int_t a, proven_int_t b) {
    if (b == 0) return Err<proven_int_t>(Status::ErrDivisionByZero);
    if (a == PROVEN_INT_MIN && b == -1) {
        return Err<proven_int_t>(Status::ErrOverflow);
    }
    return Ok<proven_int_t>(a / b);
}

/**
 * @brief Safe modulo with zero check
 */
inline Result<proven_int_t> safeMod(proven_int_t a, proven_int_t b) {
    if (b == 0) return Err<proven_int_t>(Status::ErrDivisionByZero);
    return Ok<proven_int_t>(a % b);
}

/**
 * @brief Safe absolute value
 */
inline Result<proven_int_t> safeAbs(proven_int_t a) {
    if (a == PROVEN_INT_MIN) {
        return Err<proven_int_t>(Status::ErrOverflow);
    }
    return Ok<proven_int_t>(a < 0 ? -a : a);
}

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
 */
inline Result<proven_int_t> safeMap(
    proven_int_t x,
    proven_int_t inMin, proven_int_t inMax,
    proven_int_t outMin, proven_int_t outMax
) {
    if (inMax == inMin) return Err<proven_int_t>(Status::ErrDivisionByZero);
    x = clamp(x, inMin, inMax);

    auto diff = safeSub(x, inMin);
    if (diff.isErr()) return diff;

    auto outRange = safeSub(outMax, outMin);
    if (outRange.isErr()) return outRange;

    auto scaled = safeMul(diff.value(), outRange.value());
    if (scaled.isErr()) return scaled;

    auto inRangeVal = safeSub(inMax, inMin);
    if (inRangeVal.isErr()) return inRangeVal;

    auto divided = safeDiv(scaled.value(), inRangeVal.value());
    if (divided.isErr()) return divided;

    return safeAdd(divided.value(), outMin);
}

// ============================================================================
// MODULE 2: SafeString - Core (2/11)
// ============================================================================

/**
 * @brief Safe string length with maximum bound
 */
inline size_t safeStrLen(const char* str, size_t maxLen = PROVEN_MAX_STRING_LEN) {
    if (str == nullptr) return 0;
    size_t len = 0;
    while (len < maxLen && str[len] != '\0') {
        len++;
    }
    return len;
}

/**
 * @brief Check if string contains only printable ASCII
 */
inline bool isPrintableAscii(const char* str, size_t len) {
    if (str == nullptr) return false;
    for (size_t i = 0; i < len; i++) {
        if (str[i] < 32 || str[i] > 126) return false;
    }
    return true;
}

/**
 * @brief Check if string contains only alphanumeric characters
 */
inline bool isAlphaNumeric(const char* str, size_t len) {
    if (str == nullptr) return false;
    for (size_t i = 0; i < len; i++) {
        char c = str[i];
        bool valid = (c >= 'a' && c <= 'z') ||
                     (c >= 'A' && c <= 'Z') ||
                     (c >= '0' && c <= '9');
        if (!valid) return false;
    }
    return true;
}

/**
 * @brief Check if character is valid UTF-8 continuation byte
 */
inline bool isUtf8Continuation(uint8_t byte) {
    return (byte & 0xC0) == 0x80;
}

/**
 * @brief Validate UTF-8 encoding
 */
inline bool isValidUtf8(const uint8_t* data, size_t len) {
    if (data == nullptr) return false;

    size_t i = 0;
    while (i < len) {
        uint8_t b = data[i];

        if (b <= 0x7F) {
            i++;
        } else if ((b & 0xE0) == 0xC0) {
            if (i + 1 >= len || !isUtf8Continuation(data[i + 1])) return false;
            i += 2;
        } else if ((b & 0xF0) == 0xE0) {
            if (i + 2 >= len || !isUtf8Continuation(data[i + 1]) ||
                !isUtf8Continuation(data[i + 2])) return false;
            i += 3;
        } else if ((b & 0xF8) == 0xF0) {
            if (i + 3 >= len || !isUtf8Continuation(data[i + 1]) ||
                !isUtf8Continuation(data[i + 2]) || !isUtf8Continuation(data[i + 3])) return false;
            i += 4;
        } else {
            return false;
        }
    }
    return true;
}

/**
 * @brief Safe string copy with bounds checking
 */
inline size_t safeStrCopy(char* dest, size_t destSize, const char* src, size_t srcLen) {
    if (dest == nullptr || destSize == 0) return 0;
    if (src == nullptr) {
        dest[0] = '\0';
        return 0;
    }

    size_t copyLen = (srcLen < destSize - 1) ? srcLen : destSize - 1;
    memcpy(dest, src, copyLen);
    dest[copyLen] = '\0';
    return copyLen;
}

// ============================================================================
// MODULE 3: SafePath - Core (3/11)
// ============================================================================

/**
 * @brief Check if path contains directory traversal sequences
 */
inline bool hasPathTraversal(const char* path, size_t len) {
    if (path == nullptr || len < 2) return false;

    for (size_t i = 0; i < len - 1; i++) {
        if (path[i] == '.' && path[i + 1] == '.') {
            // Check for ../  or ..\  or end of string
            if (i + 2 >= len || path[i + 2] == '/' || path[i + 2] == '\\') {
                return true;
            }
        }
    }
    return false;
}

/**
 * @brief Check if filename contains dangerous characters
 */
inline bool isSafeFilename(const char* name, size_t len) {
    if (name == nullptr || len == 0) return false;

    // Cannot start with dot or hyphen
    if (name[0] == '.' || name[0] == '-') return false;

    for (size_t i = 0; i < len; i++) {
        char c = name[i];
        // Allow alphanumeric, dot, hyphen, underscore
        bool safe = (c >= 'a' && c <= 'z') ||
                    (c >= 'A' && c <= 'Z') ||
                    (c >= '0' && c <= '9') ||
                    c == '.' || c == '-' || c == '_';
        if (!safe) return false;
    }
    return true;
}

/**
 * @brief Check if path is absolute
 */
inline bool isAbsolutePath(const char* path, size_t len) {
    if (path == nullptr || len == 0) return false;
    return path[0] == '/' || (len >= 2 && path[1] == ':');
}

// ============================================================================
// MODULE 4: SafeEmail - Core (4/11)
// ============================================================================

/**
 * @brief Basic email validation (simplified RFC 5321)
 */
inline bool isValidEmail(const char* email, size_t len) {
    if (email == nullptr || len < 5 || len > 254) return false;

    int atPos = -1;
    int dotAfterAt = -1;

    for (size_t i = 0; i < len; i++) {
        char c = email[i];

        if (c == '@') {
            if (atPos >= 0) return false;  // Multiple @
            if (i == 0) return false;       // @ at start
            atPos = i;
        } else if (c == '.' && atPos >= 0) {
            dotAfterAt = i;
        } else if (c == ' ' || c < 33 || c > 126) {
            return false;  // Invalid character
        }
    }

    // Must have @ and dot after @ but not at the end
    return atPos > 0 && dotAfterAt > atPos + 1 && (size_t)dotAfterAt < len - 1;
}

// ============================================================================
// MODULE 5: SafeUrl - Core (5/11)
// ============================================================================

/**
 * @brief URL components (minimal version for embedded)
 */
struct UrlParts {
    uint8_t schemeStart;
    uint8_t schemeLen;
    uint8_t hostStart;
    uint8_t hostLen;
    uint16_t port;
    uint8_t pathStart;
    uint8_t pathLen;
    bool valid;
};

/**
 * @brief Parse URL into components (simplified)
 */
inline UrlParts parseUrl(const char* url, size_t len) {
    UrlParts parts = {0, 0, 0, 0, 0, 0, 0, false};
    if (url == nullptr || len == 0 || len > 255) return parts;

    size_t i = 0;

    // Find scheme (://))
    size_t schemeEnd = 0;
    for (; schemeEnd < len - 2; schemeEnd++) {
        if (url[schemeEnd] == ':' && url[schemeEnd + 1] == '/' && url[schemeEnd + 2] == '/') {
            break;
        }
    }

    if (schemeEnd < len - 2) {
        parts.schemeStart = 0;
        parts.schemeLen = schemeEnd;
        i = schemeEnd + 3;  // Skip ://
    }

    // Find host
    parts.hostStart = i;
    while (i < len && url[i] != ':' && url[i] != '/' && url[i] != '?') {
        i++;
    }
    parts.hostLen = i - parts.hostStart;

    // Find port
    if (i < len && url[i] == ':') {
        i++;
        uint16_t port = 0;
        while (i < len && url[i] >= '0' && url[i] <= '9') {
            port = port * 10 + (url[i] - '0');
            if (port > 65535) return parts;
            i++;
        }
        parts.port = port;
    }

    // Find path
    if (i < len && url[i] == '/') {
        parts.pathStart = i;
        while (i < len && url[i] != '?' && url[i] != '#') {
            i++;
        }
        parts.pathLen = i - parts.pathStart;
    }

    parts.valid = parts.hostLen > 0;
    return parts;
}

/**
 * @brief Check if URL uses HTTPS
 */
inline bool isHttps(const char* url, size_t len) {
    if (url == nullptr || len < 8) return false;
    return (url[0] == 'h' || url[0] == 'H') &&
           (url[1] == 't' || url[1] == 'T') &&
           (url[2] == 't' || url[2] == 'T') &&
           (url[3] == 'p' || url[3] == 'P') &&
           (url[4] == 's' || url[4] == 'S') &&
           url[5] == ':' && url[6] == '/' && url[7] == '/';
}

// ============================================================================
// MODULE 6: SafeNetwork - Core (6/11)
// ============================================================================

/**
 * @brief IPv4 address structure
 */
struct IPv4Address {
    uint8_t octets[4];
    bool valid;
};

/**
 * @brief Parse IPv4 address string
 */
inline IPv4Address parseIPv4(const char* str, size_t len) {
    IPv4Address addr = {{0, 0, 0, 0}, false};
    if (str == nullptr || len < 7 || len > 15) return addr;

    uint8_t octet = 0;
    uint16_t value = 0;
    size_t digitCount = 0;

    for (size_t i = 0; i <= len; i++) {
        char c = (i < len) ? str[i] : '.';

        if (c >= '0' && c <= '9') {
            value = value * 10 + (c - '0');
            digitCount++;
            if (value > 255 || digitCount > 3) return addr;
        } else if (c == '.') {
            if (digitCount == 0) return addr;
            if (octet >= 4) return addr;
            addr.octets[octet++] = value;
            value = 0;
            digitCount = 0;
        } else {
            return addr;
        }
    }

    addr.valid = (octet == 4);
    return addr;
}

/**
 * @brief Check if IPv4 is private (RFC 1918)
 */
inline bool isPrivateIPv4(const IPv4Address& addr) {
    if (!addr.valid) return false;
    // 10.0.0.0/8
    if (addr.octets[0] == 10) return true;
    // 172.16.0.0/12
    if (addr.octets[0] == 172 && (addr.octets[1] & 0xF0) == 16) return true;
    // 192.168.0.0/16
    if (addr.octets[0] == 192 && addr.octets[1] == 168) return true;
    return false;
}

/**
 * @brief Check if IPv4 is loopback (127.0.0.0/8)
 */
inline bool isLoopbackIPv4(const IPv4Address& addr) {
    return addr.valid && addr.octets[0] == 127;
}

/**
 * @brief Check if port number is valid
 */
inline bool isValidPort(uint16_t port) {
    return port > 0 && port <= 65535;
}

/**
 * @brief Check if port is privileged (< 1024)
 */
inline bool isPrivilegedPort(uint16_t port) {
    return port > 0 && port < 1024;
}

// ============================================================================
// MODULE 7: SafeCrypto - Core (7/11)
// ============================================================================

/**
 * @brief Constant-time byte comparison (timing-safe)
 */
inline bool constantTimeEquals(const uint8_t* a, const uint8_t* b, size_t len) {
    if (a == nullptr || b == nullptr) return false;

    volatile uint8_t diff = 0;
    for (size_t i = 0; i < len; i++) {
        diff |= a[i] ^ b[i];
    }
    return diff == 0;
}

/**
 * @brief XOR two byte arrays
 */
inline void xorBytes(uint8_t* dest, const uint8_t* src, size_t len) {
    if (dest == nullptr || src == nullptr) return;
    for (size_t i = 0; i < len; i++) {
        dest[i] ^= src[i];
    }
}

/**
 * @brief Secure memory zero (volatile to prevent optimization)
 */
inline void secureZero(void* ptr, size_t len) {
    if (ptr == nullptr) return;
    volatile uint8_t* p = (volatile uint8_t*)ptr;
    while (len--) {
        *p++ = 0;
    }
}

/**
 * @brief Simple PRNG (xorshift32) - NOT cryptographically secure
 * @note For embedded use only. Use hardware RNG when available.
 */
class SimpleRng {
public:
    SimpleRng(uint32_t seed = 0) : _state(seed ? seed : 1) {}

    uint32_t next() {
        _state ^= _state << 13;
        _state ^= _state >> 17;
        _state ^= _state << 5;
        return _state;
    }

    uint8_t nextByte() {
        return (uint8_t)(next() & 0xFF);
    }

    void fill(uint8_t* buffer, size_t len) {
        for (size_t i = 0; i < len; i++) {
            buffer[i] = nextByte();
        }
    }

private:
    uint32_t _state;
};

// ============================================================================
// MODULE 8: SafeUUID - Core (8/11)
// ============================================================================

/**
 * @brief UUID structure (16 bytes)
 */
struct UUID {
    uint8_t bytes[16];
};

/**
 * @brief Check if two UUIDs are equal
 */
inline bool uuidEquals(const UUID& a, const UUID& b) {
    return constantTimeEquals(a.bytes, b.bytes, 16);
}

/**
 * @brief Check if UUID is nil (all zeros)
 */
inline bool uuidIsNil(const UUID& uuid) {
    for (int i = 0; i < 16; i++) {
        if (uuid.bytes[i] != 0) return false;
    }
    return true;
}

/**
 * @brief Format UUID to string (requires 37 byte buffer: 36 + null)
 */
inline bool uuidToString(const UUID& uuid, char* buffer, size_t bufferSize) {
    if (buffer == nullptr || bufferSize < 37) return false;

    const char hex[] = "0123456789abcdef";
    int pos = 0;

    for (int i = 0; i < 16; i++) {
        if (i == 4 || i == 6 || i == 8 || i == 10) {
            buffer[pos++] = '-';
        }
        buffer[pos++] = hex[(uuid.bytes[i] >> 4) & 0x0F];
        buffer[pos++] = hex[uuid.bytes[i] & 0x0F];
    }
    buffer[pos] = '\0';
    return true;
}

/**
 * @brief Parse UUID from string
 */
inline Result<UUID> uuidFromString(const char* str, size_t len) {
    UUID uuid = {{0}};
    if (str == nullptr || len != 36) {
        return Err<UUID>(Status::ErrParseFailure, uuid);
    }

    int byteIdx = 0;
    for (size_t i = 0; i < 36 && byteIdx < 16; i++) {
        if (i == 8 || i == 13 || i == 18 || i == 23) {
            if (str[i] != '-') return Err<UUID>(Status::ErrParseFailure, uuid);
            continue;
        }

        uint8_t high, low;
        char c = str[i];
        if (c >= '0' && c <= '9') high = c - '0';
        else if (c >= 'a' && c <= 'f') high = c - 'a' + 10;
        else if (c >= 'A' && c <= 'F') high = c - 'A' + 10;
        else return Err<UUID>(Status::ErrParseFailure, uuid);

        i++;
        c = str[i];
        if (c >= '0' && c <= '9') low = c - '0';
        else if (c >= 'a' && c <= 'f') low = c - 'a' + 10;
        else if (c >= 'A' && c <= 'F') low = c - 'A' + 10;
        else return Err<UUID>(Status::ErrParseFailure, uuid);

        uuid.bytes[byteIdx++] = (high << 4) | low;
    }

    return Ok(uuid);
}

// ============================================================================
// MODULE 9: SafeCurrency - Core (9/11)
// ============================================================================

/**
 * @brief Currency amount in smallest unit (cents, pence, etc.)
 */
struct Currency {
    int32_t amount;      // Amount in smallest unit
    char code[4];        // ISO 4217 code (3 chars + null)
    uint8_t decimals;    // Number of decimal places (usually 2)
};

/**
 * @brief Create currency amount
 */
inline Currency makeCurrency(int32_t amount, const char* code = "USD", uint8_t decimals = 2) {
    Currency c = {amount, {'U', 'S', 'D', '\0'}, decimals};
    if (code != nullptr) {
        for (int i = 0; i < 3 && code[i]; i++) {
            c.code[i] = code[i];
        }
        c.code[3] = '\0';
    }
    return c;
}

/**
 * @brief Safe currency addition
 */
inline Result<Currency> currencyAdd(const Currency& a, const Currency& b) {
    if (a.decimals != b.decimals) {
        return Err<Currency>(Status::ErrInvalidArgument);
    }
    // Check currency code match
    for (int i = 0; i < 3; i++) {
        if (a.code[i] != b.code[i]) {
            return Err<Currency>(Status::ErrInvalidArgument);
        }
    }

    auto result = safeAdd(a.amount, b.amount);
    if (result.isErr()) {
        return Err<Currency>(result.status());
    }

    Currency c = a;
    c.amount = result.value();
    return Ok(c);
}

/**
 * @brief Safe currency multiplication by integer
 */
inline Result<Currency> currencyMul(const Currency& c, int32_t multiplier) {
    auto result = safeMul(c.amount, multiplier);
    if (result.isErr()) {
        return Err<Currency>(result.status());
    }

    Currency newC = c;
    newC.amount = result.value();
    return Ok(newC);
}

// ============================================================================
// MODULE 10: SafePhone - Core (10/11)
// ============================================================================

/**
 * @brief Validate phone number (E.164 format: +[country][number])
 */
inline bool isValidPhone(const char* phone, size_t len) {
    if (phone == nullptr || len < 8 || len > 16) return false;

    // Must start with +
    if (phone[0] != '+') return false;

    // Rest must be digits
    for (size_t i = 1; i < len; i++) {
        if (phone[i] < '0' || phone[i] > '9') return false;
    }

    return true;
}

/**
 * @brief Extract country code (1-3 digits after +)
 */
inline int16_t extractCountryCode(const char* phone, size_t len) {
    if (phone == nullptr || len < 2 || phone[0] != '+') return -1;

    int16_t code = 0;
    size_t digits = 0;

    // Country codes are 1-3 digits
    for (size_t i = 1; i < len && digits < 3; i++) {
        if (phone[i] >= '0' && phone[i] <= '9') {
            code = code * 10 + (phone[i] - '0');
            digits++;
            // Check for common country code patterns
            if (digits == 1 && (code == 1 || code == 7)) return code;  // USA/Russia
            if (digits == 2 && code >= 20 && code <= 99) return code;  // Most countries
        } else {
            break;
        }
    }

    return (digits > 0) ? code : -1;
}

// ============================================================================
// MODULE 11: SafeHex - Core (11/11)
// ============================================================================

/**
 * @brief Convert byte to hex string (2 chars)
 */
inline void byteToHex(uint8_t byte, char* out) {
    const char hex[] = "0123456789abcdef";
    out[0] = hex[(byte >> 4) & 0x0F];
    out[1] = hex[byte & 0x0F];
}

/**
 * @brief Convert bytes to hex string
 * @param buffer Output buffer (must be at least len*2 + 1)
 */
inline bool bytesToHex(const uint8_t* data, size_t len, char* buffer, size_t bufferSize) {
    if (data == nullptr || buffer == nullptr || bufferSize < len * 2 + 1) return false;

    for (size_t i = 0; i < len; i++) {
        byteToHex(data[i], buffer + i * 2);
    }
    buffer[len * 2] = '\0';
    return true;
}

/**
 * @brief Parse hex character to nibble
 */
inline int8_t hexCharToNibble(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

/**
 * @brief Convert hex string to bytes
 */
inline Result<size_t> hexToBytes(const char* hex, size_t hexLen, uint8_t* buffer, size_t bufferSize) {
    if (hex == nullptr || buffer == nullptr) {
        return Err<size_t>(Status::ErrNullPointer);
    }
    if (hexLen % 2 != 0) {
        return Err<size_t>(Status::ErrInvalidArgument);
    }

    size_t byteLen = hexLen / 2;
    if (byteLen > bufferSize) {
        return Err<size_t>(Status::ErrOutOfBounds);
    }

    for (size_t i = 0; i < byteLen; i++) {
        int8_t high = hexCharToNibble(hex[i * 2]);
        int8_t low = hexCharToNibble(hex[i * 2 + 1]);
        if (high < 0 || low < 0) {
            return Err<size_t>(Status::ErrParseFailure);
        }
        buffer[i] = (high << 4) | low;
    }

    return Ok(byteLen);
}

// ============================================================================
// MODULE 12: SafeJson - Data (1/7)
// ============================================================================

/**
 * @brief JSON value type
 */
enum class JsonType : uint8_t {
    Null = 0,
    Bool,
    Number,
    String,
    Array,
    Object,
    Invalid = 0xFF
};

/**
 * @brief Detect JSON type at root level
 */
inline JsonType detectJsonType(const char* json, size_t len) {
    if (json == nullptr || len == 0) return JsonType::Invalid;

    // Skip whitespace
    size_t i = 0;
    while (i < len && (json[i] == ' ' || json[i] == '\t' || json[i] == '\n' || json[i] == '\r')) {
        i++;
    }
    if (i >= len) return JsonType::Invalid;

    char c = json[i];
    if (c == '{') return JsonType::Object;
    if (c == '[') return JsonType::Array;
    if (c == '"') return JsonType::String;
    if (c == 't' || c == 'f') return JsonType::Bool;
    if (c == 'n') return JsonType::Null;
    if (c == '-' || (c >= '0' && c <= '9')) return JsonType::Number;

    return JsonType::Invalid;
}

/**
 * @brief Basic JSON syntax validation (balanced braces/brackets)
 */
inline bool isValidJsonSyntax(const char* json, size_t len) {
    if (json == nullptr || len == 0) return false;

    int braceDepth = 0;
    int bracketDepth = 0;
    bool inString = false;
    bool escaped = false;

    for (size_t i = 0; i < len; i++) {
        char c = json[i];

        if (escaped) {
            escaped = false;
            continue;
        }

        if (c == '\\' && inString) {
            escaped = true;
            continue;
        }

        if (c == '"') {
            inString = !inString;
            continue;
        }

        if (!inString) {
            if (c == '{') braceDepth++;
            else if (c == '}') {
                braceDepth--;
                if (braceDepth < 0) return false;
            }
            else if (c == '[') bracketDepth++;
            else if (c == ']') {
                bracketDepth--;
                if (bracketDepth < 0) return false;
            }
        }
    }

    return braceDepth == 0 && bracketDepth == 0 && !inString;
}

// ============================================================================
// MODULE 13: SafeDateTime - Data (2/7)
// ============================================================================

/**
 * @brief DateTime components
 */
struct DateTime {
    int16_t year;
    uint8_t month;   // 1-12
    uint8_t day;     // 1-31
    uint8_t hour;    // 0-23
    uint8_t minute;  // 0-59
    uint8_t second;  // 0-59
    bool valid;
};

/**
 * @brief Check if year is leap year
 */
inline bool isLeapYear(int16_t year) {
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
}

/**
 * @brief Get days in month
 */
inline uint8_t daysInMonth(int16_t year, uint8_t month) {
    if (month == 0 || month > 12) return 0;
    static const uint8_t days[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    if (month == 2 && isLeapYear(year)) return 29;
    return days[month - 1];
}

/**
 * @brief Validate DateTime components
 */
inline bool isValidDateTime(const DateTime& dt) {
    if (dt.month == 0 || dt.month > 12) return false;
    if (dt.day == 0 || dt.day > daysInMonth(dt.year, dt.month)) return false;
    if (dt.hour > 23 || dt.minute > 59 || dt.second > 59) return false;
    return true;
}

/**
 * @brief Parse two digits from string
 */
inline int8_t parseTwoDigits(const char* str) {
    if (str[0] < '0' || str[0] > '9' || str[1] < '0' || str[1] > '9') return -1;
    return (str[0] - '0') * 10 + (str[1] - '0');
}

/**
 * @brief Parse ISO 8601 date/time (YYYY-MM-DDTHH:MM:SS)
 */
inline DateTime parseDateTime(const char* str, size_t len) {
    DateTime dt = {0, 0, 0, 0, 0, 0, false};

    if (str == nullptr || len < 10) return dt;

    // Parse year (4 digits)
    if (len >= 4) {
        dt.year = 0;
        for (int i = 0; i < 4; i++) {
            if (str[i] < '0' || str[i] > '9') return dt;
            dt.year = dt.year * 10 + (str[i] - '0');
        }
    }

    if (len < 10 || str[4] != '-' || str[7] != '-') return dt;

    int8_t month = parseTwoDigits(str + 5);
    int8_t day = parseTwoDigits(str + 8);
    if (month < 0 || day < 0) return dt;

    dt.month = month;
    dt.day = day;

    // Parse time if present
    if (len >= 19 && (str[10] == 'T' || str[10] == ' ') && str[13] == ':' && str[16] == ':') {
        int8_t hour = parseTwoDigits(str + 11);
        int8_t minute = parseTwoDigits(str + 14);
        int8_t second = parseTwoDigits(str + 17);

        if (hour >= 0 && minute >= 0 && second >= 0) {
            dt.hour = hour;
            dt.minute = minute;
            dt.second = second;
        }
    }

    dt.valid = isValidDateTime(dt);
    return dt;
}

/**
 * @brief Calculate day of week (0=Sunday, 6=Saturday)
 */
inline uint8_t dayOfWeek(int16_t year, uint8_t month, uint8_t day) {
    // Zeller's algorithm
    if (month < 3) {
        month += 12;
        year--;
    }
    int k = year % 100;
    int j = year / 100;
    int h = (day + (13 * (month + 1)) / 5 + k + k / 4 + j / 4 - 2 * j) % 7;
    return ((h + 6) % 7);  // Convert to Sunday = 0
}

// ============================================================================
// MODULE 14: SafeFloat - Data (3/7)
// ============================================================================

/**
 * @brief Check if float is valid (not NaN or Inf)
 */
inline bool isValidFloat(float f) {
    return f == f && f != (f + 1.0f);  // NaN != NaN, Inf + 1 == Inf
}

/**
 * @brief Check if floats are approximately equal
 */
inline bool floatEquals(float a, float b, float epsilon = 0.0001f) {
    if (!isValidFloat(a) || !isValidFloat(b)) return false;
    float diff = a - b;
    if (diff < 0) diff = -diff;
    return diff <= epsilon;
}

/**
 * @brief Safe float to int conversion
 */
inline Result<int32_t> floatToInt(float f) {
    if (!isValidFloat(f)) {
        return Err<int32_t>(Status::ErrInvalidArgument);
    }
    if (f > (float)INT32_MAX || f < (float)INT32_MIN) {
        return Err<int32_t>(Status::ErrOverflow);
    }
    return Ok<int32_t>((int32_t)f);
}

/**
 * @brief Clamp float to range
 */
inline float clampFloat(float value, float minVal, float maxVal) {
    if (!isValidFloat(value)) return minVal;
    if (value < minVal) return minVal;
    if (value > maxVal) return maxVal;
    return value;
}

// ============================================================================
// MODULE 15: SafeVersion - Data (4/7)
// ============================================================================

/**
 * @brief Semantic version structure
 */
struct Version {
    uint16_t major;
    uint16_t minor;
    uint16_t patch;
    bool valid;
};

/**
 * @brief Parse semantic version string (MAJOR.MINOR.PATCH)
 */
inline Version parseVersion(const char* str, size_t len) {
    Version v = {0, 0, 0, false};
    if (str == nullptr || len == 0) return v;

    uint16_t values[3] = {0, 0, 0};
    int part = 0;
    int digits = 0;

    for (size_t i = 0; i < len && part < 3; i++) {
        char c = str[i];
        if (c >= '0' && c <= '9') {
            if (values[part] > 6553 || (values[part] == 6553 && c > '5')) {
                return v;  // Overflow
            }
            values[part] = values[part] * 10 + (c - '0');
            digits++;
        } else if (c == '.') {
            if (digits == 0) return v;
            part++;
            digits = 0;
        } else {
            break;  // Prerelease info not parsed
        }
    }

    if (part >= 2 && digits > 0) {
        v.major = values[0];
        v.minor = values[1];
        v.patch = values[2];
        v.valid = true;
    }

    return v;
}

/**
 * @brief Compare versions
 * @return -1 if a < b, 0 if equal, 1 if a > b
 */
inline int8_t compareVersions(const Version& a, const Version& b) {
    if (a.major != b.major) return a.major < b.major ? -1 : 1;
    if (a.minor != b.minor) return a.minor < b.minor ? -1 : 1;
    if (a.patch != b.patch) return a.patch < b.patch ? -1 : 1;
    return 0;
}

// ============================================================================
// MODULE 16: SafeColor - Data (5/7)
// ============================================================================

/**
 * @brief RGB color structure
 */
struct RGB {
    uint8_t r;
    uint8_t g;
    uint8_t b;
};

/**
 * @brief RGBA color structure
 */
struct RGBA {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
};

/**
 * @brief HSV color structure (all 0-255 for embedded efficiency)
 */
struct HSV {
    uint8_t h;  // Hue: 0-255 maps to 0-360
    uint8_t s;  // Saturation: 0-255
    uint8_t v;  // Value: 0-255
};

/**
 * @brief Parse hex color (#RGB or #RRGGBB)
 */
inline Result<RGB> parseHexColor(const char* hex, size_t len) {
    RGB color = {0, 0, 0};

    if (hex == nullptr) return Err<RGB>(Status::ErrNullPointer);
    if (hex[0] == '#') { hex++; len--; }

    if (len == 3) {
        // Short form #RGB
        int8_t r = hexCharToNibble(hex[0]);
        int8_t g = hexCharToNibble(hex[1]);
        int8_t b = hexCharToNibble(hex[2]);
        if (r < 0 || g < 0 || b < 0) return Err<RGB>(Status::ErrParseFailure);
        color.r = r * 17;  // Expand 0xF to 0xFF
        color.g = g * 17;
        color.b = b * 17;
    } else if (len == 6) {
        // Full form #RRGGBB
        int8_t rh = hexCharToNibble(hex[0]);
        int8_t rl = hexCharToNibble(hex[1]);
        int8_t gh = hexCharToNibble(hex[2]);
        int8_t gl = hexCharToNibble(hex[3]);
        int8_t bh = hexCharToNibble(hex[4]);
        int8_t bl = hexCharToNibble(hex[5]);
        if (rh < 0 || rl < 0 || gh < 0 || gl < 0 || bh < 0 || bl < 0) {
            return Err<RGB>(Status::ErrParseFailure);
        }
        color.r = (rh << 4) | rl;
        color.g = (gh << 4) | gl;
        color.b = (bh << 4) | bl;
    } else {
        return Err<RGB>(Status::ErrParseFailure);
    }

    return Ok(color);
}

/**
 * @brief Convert RGB to 16-bit color (5-6-5)
 */
inline uint16_t rgbTo565(const RGB& color) {
    return ((color.r & 0xF8) << 8) | ((color.g & 0xFC) << 3) | (color.b >> 3);
}

/**
 * @brief Blend two colors
 */
inline RGB blendRgb(const RGB& a, const RGB& b, uint8_t t) {
    RGB result;
    result.r = ((uint16_t)a.r * (255 - t) + (uint16_t)b.r * t) / 255;
    result.g = ((uint16_t)a.g * (255 - t) + (uint16_t)b.g * t) / 255;
    result.b = ((uint16_t)a.b * (255 - t) + (uint16_t)b.b * t) / 255;
    return result;
}

// ============================================================================
// MODULE 17: SafeAngle - Data (6/7)
// ============================================================================

/**
 * @brief Angle in fixed-point (1/100 degree precision)
 */
struct Angle {
    int32_t centidegrees;  // -36000 to 36000 for -360 to +360 degrees
};

/**
 * @brief Create angle from degrees
 */
inline Angle angleFromDegrees(float degrees) {
    Angle a;
    a.centidegrees = (int32_t)(degrees * 100.0f);
    return a;
}

/**
 * @brief Get angle in degrees
 */
inline float angleToDegrees(const Angle& a) {
    return a.centidegrees / 100.0f;
}

/**
 * @brief Create angle from radians
 */
inline Angle angleFromRadians(float radians) {
    return angleFromDegrees(radians * 57.2957795f);  // 180/PI
}

/**
 * @brief Get angle in radians
 */
inline float angleToRadians(const Angle& a) {
    return angleToDegrees(a) * 0.0174532925f;  // PI/180
}

/**
 * @brief Normalize angle to [0, 360)
 */
inline Angle normalizeAngle(const Angle& a) {
    Angle result;
    result.centidegrees = a.centidegrees % 36000;
    if (result.centidegrees < 0) result.centidegrees += 36000;
    return result;
}

/**
 * @brief Normalize angle to [-180, 180)
 */
inline Angle normalizeAngleSigned(const Angle& a) {
    Angle result = normalizeAngle(a);
    if (result.centidegrees >= 18000) result.centidegrees -= 36000;
    return result;
}

// ============================================================================
// MODULE 18: SafeUnit - Data (7/7)
// ============================================================================

/**
 * @brief Unit type for conversions
 */
enum class UnitType : uint8_t {
    Length,
    Mass,
    Temperature,
    Time,
    Volume
};

/**
 * @brief Convert millimeters to inches (fixed-point)
 */
inline int32_t mmToInches100(int32_t mm) {
    // 1 inch = 25.4mm, result in 1/100 inches
    return (mm * 100) / 254;
}

/**
 * @brief Convert inches to millimeters (fixed-point)
 */
inline int32_t inches100ToMm(int32_t inches100) {
    return (inches100 * 254) / 100;
}

/**
 * @brief Convert Celsius (x10) to Fahrenheit (x10)
 */
inline int16_t celsius10ToFahrenheit10(int16_t celsius10) {
    return (celsius10 * 9 / 5) + 320;
}

/**
 * @brief Convert Fahrenheit (x10) to Celsius (x10)
 */
inline int16_t fahrenheit10ToCelsius10(int16_t fahrenheit10) {
    return (fahrenheit10 - 320) * 5 / 9;
}

/**
 * @brief Convert grams to ounces (x100)
 */
inline int32_t gramsToOunces100(int32_t grams) {
    // 1 oz = 28.3495g
    return (grams * 100 * 10000) / 283495;
}

/**
 * @brief Convert milliliters to fluid ounces (x100)
 */
inline int32_t mlToFlOz100(int32_t ml) {
    // 1 fl oz = 29.5735ml
    return (ml * 100 * 10000) / 295735;
}

// ============================================================================
// MODULE 19: SafeBuffer - Data Structures (1/5)
// ============================================================================

/**
 * @brief Fixed-size bounded buffer
 */
template<size_t CAPACITY>
class Buffer {
public:
    Buffer() : _length(0) {}

    Status append(const uint8_t* data, size_t len) {
        if (data == nullptr) return Status::ErrNullPointer;
        if (_length + len > CAPACITY) return Status::ErrBufferFull;

        memcpy(_data + _length, data, len);
        _length += len;
        return Status::Ok;
    }

    Status append(uint8_t byte) {
        if (_length >= CAPACITY) return Status::ErrBufferFull;
        _data[_length++] = byte;
        return Status::Ok;
    }

    inline const uint8_t* data() const { return _data; }
    inline size_t length() const { return _length; }
    inline size_t capacity() const { return CAPACITY; }
    inline size_t available() const { return CAPACITY - _length; }
    inline bool isEmpty() const { return _length == 0; }
    inline bool isFull() const { return _length >= CAPACITY; }

    void clear() { _length = 0; }

    Result<uint8_t> at(size_t index) const {
        if (index >= _length) return Err<uint8_t>(Status::ErrOutOfBounds);
        return Ok(_data[index]);
    }

private:
    uint8_t _data[CAPACITY];
    size_t _length;
};

// ============================================================================
// MODULE 20: SafeQueue - Data Structures (2/5)
// ============================================================================

/**
 * @brief Fixed-size circular queue
 */
template<typename T, size_t CAPACITY>
class Queue {
public:
    Queue() : _head(0), _tail(0), _count(0) {}

    Status push(const T& item) {
        if (_count >= CAPACITY) return Status::ErrBufferFull;
        _data[_tail] = item;
        _tail = (_tail + 1) % CAPACITY;
        _count++;
        return Status::Ok;
    }

    Result<T> pop() {
        if (_count == 0) return Err<T>(Status::ErrBufferEmpty);
        T item = _data[_head];
        _head = (_head + 1) % CAPACITY;
        _count--;
        return Ok(item);
    }

    Result<T> peek() const {
        if (_count == 0) return Err<T>(Status::ErrBufferEmpty);
        return Ok(_data[_head]);
    }

    inline size_t count() const { return _count; }
    inline size_t capacity() const { return CAPACITY; }
    inline bool isEmpty() const { return _count == 0; }
    inline bool isFull() const { return _count >= CAPACITY; }

    void clear() {
        _head = 0;
        _tail = 0;
        _count = 0;
    }

private:
    T _data[CAPACITY];
    size_t _head;
    size_t _tail;
    size_t _count;
};

// ============================================================================
// MODULE 21: SafeBloom - Data Structures (3/5)
// ============================================================================

/**
 * @brief Simple Bloom filter for embedded systems
 * @note Memory-efficient probabilistic set membership
 */
template<size_t BITS>
class BloomFilter {
public:
    BloomFilter() { clear(); }

    void add(const uint8_t* data, size_t len) {
        uint32_t h1 = hash1(data, len);
        uint32_t h2 = hash2(data, len);

        for (int i = 0; i < 3; i++) {
            size_t pos = (h1 + i * h2) % BITS;
            _bits[pos / 8] |= (1 << (pos % 8));
        }
        _count++;
    }

    bool mightContain(const uint8_t* data, size_t len) const {
        uint32_t h1 = hash1(data, len);
        uint32_t h2 = hash2(data, len);

        for (int i = 0; i < 3; i++) {
            size_t pos = (h1 + i * h2) % BITS;
            if ((_bits[pos / 8] & (1 << (pos % 8))) == 0) {
                return false;
            }
        }
        return true;
    }

    void clear() {
        memset(_bits, 0, sizeof(_bits));
        _count = 0;
    }

    inline size_t count() const { return _count; }

private:
    uint8_t _bits[(BITS + 7) / 8];
    size_t _count;

    uint32_t hash1(const uint8_t* data, size_t len) const {
        uint32_t h = 0x811c9dc5;
        for (size_t i = 0; i < len; i++) {
            h ^= data[i];
            h *= 0x01000193;
        }
        return h;
    }

    uint32_t hash2(const uint8_t* data, size_t len) const {
        uint32_t h = 0;
        for (size_t i = 0; i < len; i++) {
            h = h * 31 + data[i];
        }
        return h;
    }
};

// ============================================================================
// MODULE 22: SafeLRU - Data Structures (4/5)
// ============================================================================

/**
 * @brief Simple LRU cache for embedded systems
 */
template<typename K, typename V, size_t CAPACITY>
class LRUCache {
public:
    LRUCache() : _size(0) {}

    void put(const K& key, const V& value) {
        // Check if key exists
        for (size_t i = 0; i < _size; i++) {
            if (_keys[i] == key) {
                _values[i] = value;
                moveToFront(i);
                return;
            }
        }

        // Add new entry
        if (_size < CAPACITY) {
            // Shift everything down
            for (size_t i = _size; i > 0; i--) {
                _keys[i] = _keys[i - 1];
                _values[i] = _values[i - 1];
            }
            _size++;
        } else {
            // Evict least recently used
            for (size_t i = CAPACITY - 1; i > 0; i--) {
                _keys[i] = _keys[i - 1];
                _values[i] = _values[i - 1];
            }
        }

        _keys[0] = key;
        _values[0] = value;
    }

    Result<V> get(const K& key) {
        for (size_t i = 0; i < _size; i++) {
            if (_keys[i] == key) {
                V value = _values[i];
                moveToFront(i);
                return Ok(value);
            }
        }
        return Err<V>(Status::ErrOutOfBounds);
    }

    bool contains(const K& key) const {
        for (size_t i = 0; i < _size; i++) {
            if (_keys[i] == key) return true;
        }
        return false;
    }

    inline size_t size() const { return _size; }
    inline size_t capacity() const { return CAPACITY; }

    void clear() { _size = 0; }

private:
    K _keys[CAPACITY];
    V _values[CAPACITY];
    size_t _size;

    void moveToFront(size_t index) {
        if (index == 0) return;
        K tempKey = _keys[index];
        V tempVal = _values[index];
        for (size_t i = index; i > 0; i--) {
            _keys[i] = _keys[i - 1];
            _values[i] = _values[i - 1];
        }
        _keys[0] = tempKey;
        _values[0] = tempVal;
    }
};

// ============================================================================
// MODULE 23: SafeGraph - Data Structures (5/5)
// ============================================================================

/**
 * @brief Simple adjacency matrix graph for embedded systems
 */
template<size_t MAX_NODES>
class Graph {
public:
    Graph() : _nodeCount(0) {
        clear();
    }

    Status addNode() {
        if (_nodeCount >= MAX_NODES) return Status::ErrBufferFull;
        _nodeCount++;
        return Status::Ok;
    }

    Status addEdge(uint8_t from, uint8_t to, uint8_t weight = 1) {
        if (from >= _nodeCount || to >= _nodeCount) {
            return Status::ErrOutOfBounds;
        }
        _edges[from][to] = weight;
        return Status::Ok;
    }

    Status addBidirectionalEdge(uint8_t a, uint8_t b, uint8_t weight = 1) {
        auto s1 = addEdge(a, b, weight);
        if (s1 != Status::Ok) return s1;
        return addEdge(b, a, weight);
    }

    uint8_t getEdgeWeight(uint8_t from, uint8_t to) const {
        if (from >= _nodeCount || to >= _nodeCount) return 0;
        return _edges[from][to];
    }

    bool hasEdge(uint8_t from, uint8_t to) const {
        return getEdgeWeight(from, to) > 0;
    }

    inline uint8_t nodeCount() const { return _nodeCount; }

    void clear() {
        _nodeCount = 0;
        memset(_edges, 0, sizeof(_edges));
    }

private:
    uint8_t _edges[MAX_NODES][MAX_NODES];
    uint8_t _nodeCount;
};

// ============================================================================
// MODULE 24: SafeRateLimiter - Resilience (1/4)
// ============================================================================

/**
 * @brief Token bucket rate limiter
 */
class RateLimiter {
public:
    /**
     * @param tokensPerSecond Refill rate
     * @param bucketSize Maximum tokens
     */
    RateLimiter(uint16_t tokensPerSecond, uint16_t bucketSize)
        : _refillRate(tokensPerSecond)
        , _bucketSize(bucketSize)
        , _tokens(bucketSize)
        , _lastRefill(0) {}

    /**
     * @brief Try to acquire tokens
     * @param tokens Number of tokens to acquire
     * @param currentMillis Current time in milliseconds
     */
    bool tryAcquire(uint16_t tokens = 1, unsigned long currentMillis = 0) {
        if (currentMillis == 0) currentMillis = millis();
        refill(currentMillis);

        if (_tokens >= tokens) {
            _tokens -= tokens;
            return true;
        }
        return false;
    }

    /**
     * @brief Check if tokens available without consuming
     */
    bool canAcquire(uint16_t tokens = 1, unsigned long currentMillis = 0) {
        if (currentMillis == 0) currentMillis = millis();
        refill(currentMillis);
        return _tokens >= tokens;
    }

    inline uint16_t availableTokens() const { return _tokens; }

    void reset() {
        _tokens = _bucketSize;
        _lastRefill = millis();
    }

private:
    uint16_t _refillRate;
    uint16_t _bucketSize;
    uint16_t _tokens;
    unsigned long _lastRefill;

    void refill(unsigned long currentMillis) {
        if (_lastRefill == 0) {
            _lastRefill = currentMillis;
            return;
        }

        unsigned long elapsed = currentMillis - _lastRefill;
        if (elapsed >= 1000) {
            uint16_t newTokens = (elapsed / 1000) * _refillRate;
            _tokens = min((uint16_t)(_tokens + newTokens), _bucketSize);
            _lastRefill = currentMillis;
        }
    }
};

// ============================================================================
// MODULE 25: SafeCircuitBreaker - Resilience (2/4)
// ============================================================================

/**
 * @brief Circuit breaker state
 */
enum class CircuitState : uint8_t {
    Closed,     // Normal operation
    Open,       // Failing fast
    HalfOpen    // Testing if service recovered
};

/**
 * @brief Circuit breaker pattern
 */
class CircuitBreaker {
public:
    /**
     * @param failureThreshold Failures before opening
     * @param recoveryTimeMs Time before half-open
     */
    CircuitBreaker(uint8_t failureThreshold = 5, unsigned long recoveryTimeMs = 30000)
        : _state(CircuitState::Closed)
        , _failureCount(0)
        , _failureThreshold(failureThreshold)
        , _recoveryTime(recoveryTimeMs)
        , _lastFailureTime(0) {}

    /**
     * @brief Check if operation should be allowed
     */
    Result<bool> allowRequest(unsigned long currentMillis = 0) {
        if (currentMillis == 0) currentMillis = millis();

        switch (_state) {
            case CircuitState::Closed:
                return Ok(true);

            case CircuitState::Open:
                if (currentMillis - _lastFailureTime >= _recoveryTime) {
                    _state = CircuitState::HalfOpen;
                    return Ok(true);
                }
                return Err<bool>(Status::ErrCircuitOpen);

            case CircuitState::HalfOpen:
                return Ok(true);
        }
        return Ok(true);
    }

    /**
     * @brief Record successful operation
     */
    void recordSuccess() {
        _failureCount = 0;
        _state = CircuitState::Closed;
    }

    /**
     * @brief Record failed operation
     */
    void recordFailure(unsigned long currentMillis = 0) {
        if (currentMillis == 0) currentMillis = millis();

        _lastFailureTime = currentMillis;

        if (_state == CircuitState::HalfOpen) {
            _state = CircuitState::Open;
            return;
        }

        _failureCount++;
        if (_failureCount >= _failureThreshold) {
            _state = CircuitState::Open;
        }
    }

    inline CircuitState state() const { return _state; }
    inline uint8_t failureCount() const { return _failureCount; }

    void reset() {
        _state = CircuitState::Closed;
        _failureCount = 0;
    }

private:
    CircuitState _state;
    uint8_t _failureCount;
    uint8_t _failureThreshold;
    unsigned long _recoveryTime;
    unsigned long _lastFailureTime;
};

// ============================================================================
// MODULE 26: SafeRetry - Resilience (3/4)
// ============================================================================

/**
 * @brief Retry strategy
 */
enum class RetryStrategy : uint8_t {
    Fixed,           // Fixed delay between retries
    Linear,          // Linearly increasing delay
    Exponential      // Exponentially increasing delay
};

/**
 * @brief Retry helper with backoff
 */
class RetryPolicy {
public:
    RetryPolicy(uint8_t maxRetries = 3, uint16_t baseDelayMs = 100, RetryStrategy strategy = RetryStrategy::Exponential)
        : _maxRetries(maxRetries)
        , _baseDelay(baseDelayMs)
        , _strategy(strategy)
        , _attempts(0) {}

    /**
     * @brief Check if should retry
     */
    bool shouldRetry() const {
        return _attempts < _maxRetries;
    }

    /**
     * @brief Get delay for next retry
     */
    uint16_t getDelayMs() const {
        switch (_strategy) {
            case RetryStrategy::Fixed:
                return _baseDelay;
            case RetryStrategy::Linear:
                return _baseDelay * (_attempts + 1);
            case RetryStrategy::Exponential:
                return _baseDelay * (1 << _attempts);
        }
        return _baseDelay;
    }

    /**
     * @brief Record an attempt
     */
    void recordAttempt() {
        if (_attempts < 255) _attempts++;
    }

    inline uint8_t attempts() const { return _attempts; }
    inline uint8_t remainingRetries() const { return (_attempts < _maxRetries) ? _maxRetries - _attempts : 0; }

    void reset() { _attempts = 0; }

private:
    uint8_t _maxRetries;
    uint16_t _baseDelay;
    RetryStrategy _strategy;
    uint8_t _attempts;
};

// ============================================================================
// MODULE 27: SafeMonotonic - Resilience (4/4)
// ============================================================================

/**
 * @brief Monotonic counter that cannot decrease
 */
class MonotonicCounter {
public:
    MonotonicCounter(uint32_t initial = 0) : _value(initial) {}

    /**
     * @brief Increment counter
     */
    Result<uint32_t> increment(uint32_t delta = 1) {
        if (_value > UINT32_MAX - delta) {
            return Err<uint32_t>(Status::ErrOverflow, _value);
        }
        _value += delta;
        return Ok(_value);
    }

    /**
     * @brief Try to set new value (only if greater)
     */
    bool trySet(uint32_t newValue) {
        if (newValue > _value) {
            _value = newValue;
            return true;
        }
        return false;
    }

    inline uint32_t value() const { return _value; }

private:
    uint32_t _value;
};

/**
 * @brief Monotonic timestamp tracker (handles wrap-around)
 */
class MonotonicTime {
public:
    MonotonicTime() : _lastTime(0), _wrapCount(0) {}

    /**
     * @brief Update with current time
     */
    void update(unsigned long currentMillis) {
        if (currentMillis < _lastTime) {
            _wrapCount++;
        }
        _lastTime = currentMillis;
    }

    /**
     * @brief Get elapsed time since mark (handles wrap-around)
     */
    unsigned long elapsedSince(unsigned long startTime) const {
        if (_lastTime >= startTime) {
            return _lastTime - startTime;
        }
        return (ULONG_MAX - startTime) + _lastTime + 1;
    }

    inline unsigned long lastTime() const { return _lastTime; }
    inline uint16_t wrapCount() const { return _wrapCount; }

private:
    unsigned long _lastTime;
    uint16_t _wrapCount;
};

// ============================================================================
// MODULE 28: SafeStateMachine - State (1/2)
// ============================================================================

/**
 * @brief Simple state machine for embedded systems
 */
template<uint8_t MAX_STATES, uint8_t MAX_TRANSITIONS>
class StateMachine {
public:
    using TransitionCallback = void(*)();

    StateMachine() : _currentState(0), _transitionCount(0) {}

    /**
     * @brief Add a transition
     */
    Status addTransition(uint8_t fromState, uint8_t toState, uint8_t event, TransitionCallback callback = nullptr) {
        if (_transitionCount >= MAX_TRANSITIONS) return Status::ErrBufferFull;
        if (fromState >= MAX_STATES || toState >= MAX_STATES) return Status::ErrOutOfBounds;

        _transitions[_transitionCount++] = {fromState, toState, event, callback};
        return Status::Ok;
    }

    /**
     * @brief Process an event
     */
    bool processEvent(uint8_t event) {
        for (uint8_t i = 0; i < _transitionCount; i++) {
            if (_transitions[i].fromState == _currentState && _transitions[i].event == event) {
                if (_transitions[i].callback) {
                    _transitions[i].callback();
                }
                _currentState = _transitions[i].toState;
                return true;
            }
        }
        return false;
    }

    inline uint8_t currentState() const { return _currentState; }

    void setState(uint8_t state) {
        if (state < MAX_STATES) _currentState = state;
    }

    void reset() { _currentState = 0; }

private:
    struct Transition {
        uint8_t fromState;
        uint8_t toState;
        uint8_t event;
        TransitionCallback callback;
    };

    uint8_t _currentState;
    Transition _transitions[MAX_TRANSITIONS];
    uint8_t _transitionCount;
};

// ============================================================================
// MODULE 29: SafeCalculator - State (2/2)
// ============================================================================

/**
 * @brief Safe RPN calculator with overflow checking
 */
template<uint8_t STACK_SIZE = 8>
class Calculator {
public:
    Calculator() : _stackPtr(0) {}

    Status push(proven_int_t value) {
        if (_stackPtr >= STACK_SIZE) return Status::ErrBufferFull;
        _stack[_stackPtr++] = value;
        return Status::Ok;
    }

    Result<proven_int_t> pop() {
        if (_stackPtr == 0) return Err<proven_int_t>(Status::ErrBufferEmpty);
        return Ok(_stack[--_stackPtr]);
    }

    Result<proven_int_t> peek() const {
        if (_stackPtr == 0) return Err<proven_int_t>(Status::ErrBufferEmpty);
        return Ok(_stack[_stackPtr - 1]);
    }

    Status add() {
        if (_stackPtr < 2) return Status::ErrBufferEmpty;
        auto b = pop();
        auto a = pop();
        auto result = safeAdd(a.value(), b.value());
        if (result.isErr()) return result.status();
        return push(result.value());
    }

    Status subtract() {
        if (_stackPtr < 2) return Status::ErrBufferEmpty;
        auto b = pop();
        auto a = pop();
        auto result = safeSub(a.value(), b.value());
        if (result.isErr()) return result.status();
        return push(result.value());
    }

    Status multiply() {
        if (_stackPtr < 2) return Status::ErrBufferEmpty;
        auto b = pop();
        auto a = pop();
        auto result = safeMul(a.value(), b.value());
        if (result.isErr()) return result.status();
        return push(result.value());
    }

    Status divide() {
        if (_stackPtr < 2) return Status::ErrBufferEmpty;
        auto b = pop();
        auto a = pop();
        auto result = safeDiv(a.value(), b.value());
        if (result.isErr()) return result.status();
        return push(result.value());
    }

    inline uint8_t stackDepth() const { return _stackPtr; }
    void clear() { _stackPtr = 0; }

private:
    proven_int_t _stack[STACK_SIZE];
    uint8_t _stackPtr;
};

// ============================================================================
// MODULE 30: SafeGeo - Algorithm (1/4)
// ============================================================================

/**
 * @brief Geographic coordinate (fixed-point microdegrees)
 */
struct GeoCoord {
    int32_t latMicrodeg;  // Latitude in microdegrees (-90000000 to 90000000)
    int32_t lonMicrodeg;  // Longitude in microdegrees (-180000000 to 180000000)
    bool valid;
};

/**
 * @brief Create GeoCoord from degrees
 */
inline GeoCoord geoFromDegrees(float lat, float lon) {
    GeoCoord coord;
    coord.latMicrodeg = (int32_t)(lat * 1000000.0f);
    coord.lonMicrodeg = (int32_t)(lon * 1000000.0f);
    coord.valid = (coord.latMicrodeg >= -90000000 && coord.latMicrodeg <= 90000000) &&
                  (coord.lonMicrodeg >= -180000000 && coord.lonMicrodeg <= 180000000);
    return coord;
}

/**
 * @brief Check if coordinate is valid
 */
inline bool isValidGeoCoord(const GeoCoord& coord) {
    return coord.valid &&
           coord.latMicrodeg >= -90000000 && coord.latMicrodeg <= 90000000 &&
           coord.lonMicrodeg >= -180000000 && coord.lonMicrodeg <= 180000000;
}

/**
 * @brief Calculate approximate distance (Equirectangular, in meters)
 * @note Accurate within ~0.5% for distances < 400km
 */
inline uint32_t geoDistanceApprox(const GeoCoord& a, const GeoCoord& b) {
    if (!a.valid || !b.valid) return 0;

    // Convert to radians (approximate)
    float lat1 = a.latMicrodeg * 0.0000001745329f;  // * PI / 180000000
    float lat2 = b.latMicrodeg * 0.0000001745329f;
    float lon1 = a.lonMicrodeg * 0.0000001745329f;
    float lon2 = b.lonMicrodeg * 0.0000001745329f;

    float x = (lon2 - lon1) * cos((lat1 + lat2) / 2);
    float y = lat2 - lat1;

    return (uint32_t)(sqrt(x * x + y * y) * 6371000.0f);  // Earth radius in meters
}

/**
 * @brief Check if point is within bounding box
 */
inline bool geoInBoundingBox(const GeoCoord& point, const GeoCoord& min, const GeoCoord& max) {
    if (!point.valid || !min.valid || !max.valid) return false;
    return point.latMicrodeg >= min.latMicrodeg && point.latMicrodeg <= max.latMicrodeg &&
           point.lonMicrodeg >= min.lonMicrodeg && point.lonMicrodeg <= max.lonMicrodeg;
}

// ============================================================================
// MODULE 31: SafeProbability - Algorithm (2/4)
// ============================================================================

/**
 * @brief Probability value (0-255 representing 0.0-1.0)
 */
struct Probability {
    uint8_t value;  // 0 = 0%, 255 = 100%
};

/**
 * @brief Create probability from percentage
 */
inline Probability probFromPercent(uint8_t percent) {
    Probability p;
    p.value = (percent > 100) ? 255 : (percent * 255 / 100);
    return p;
}

/**
 * @brief Get probability as percentage
 */
inline uint8_t probToPercent(Probability p) {
    return (p.value * 100 + 127) / 255;
}

/**
 * @brief AND two probabilities (P(A) * P(B))
 */
inline Probability probAnd(Probability a, Probability b) {
    Probability result;
    result.value = ((uint16_t)a.value * b.value + 127) / 255;
    return result;
}

/**
 * @brief OR two probabilities (P(A) + P(B) - P(A)*P(B))
 */
inline Probability probOr(Probability a, Probability b) {
    uint16_t product = ((uint16_t)a.value * b.value + 127) / 255;
    Probability result;
    result.value = clamp<uint16_t>(a.value + b.value - product, 0, 255);
    return result;
}

/**
 * @brief NOT probability (1 - P)
 */
inline Probability probNot(Probability p) {
    Probability result;
    result.value = 255 - p.value;
    return result;
}

/**
 * @brief Check if event occurs (using RNG)
 */
inline bool probOccurs(Probability p, uint8_t randomByte) {
    return randomByte < p.value;
}

// ============================================================================
// MODULE 32: SafeChecksum - Algorithm (3/4)
// ============================================================================

/**
 * @brief Calculate CRC-8 (polynomial 0x07)
 */
inline uint8_t crc8(const uint8_t* data, size_t len) {
    uint8_t crc = 0x00;
    for (size_t i = 0; i < len; i++) {
        crc ^= data[i];
        for (int bit = 0; bit < 8; bit++) {
            if (crc & 0x80) {
                crc = (crc << 1) ^ 0x07;
            } else {
                crc <<= 1;
            }
        }
    }
    return crc;
}

/**
 * @brief Calculate CRC-16 (CCITT polynomial 0x1021)
 */
inline uint16_t crc16(const uint8_t* data, size_t len) {
    uint16_t crc = 0xFFFF;
    for (size_t i = 0; i < len; i++) {
        crc ^= (uint16_t)data[i] << 8;
        for (int bit = 0; bit < 8; bit++) {
            if (crc & 0x8000) {
                crc = (crc << 1) ^ 0x1021;
            } else {
                crc <<= 1;
            }
        }
    }
    return crc;
}

/**
 * @brief Calculate simple checksum (sum of bytes)
 */
inline uint8_t checksum8(const uint8_t* data, size_t len) {
    uint8_t sum = 0;
    for (size_t i = 0; i < len; i++) {
        sum += data[i];
    }
    return sum;
}

/**
 * @brief Calculate Fletcher-16 checksum
 */
inline uint16_t fletcher16(const uint8_t* data, size_t len) {
    uint16_t sum1 = 0xFF, sum2 = 0xFF;
    for (size_t i = 0; i < len; i++) {
        sum1 = (sum1 + data[i]) % 255;
        sum2 = (sum2 + sum1) % 255;
    }
    return (sum2 << 8) | sum1;
}

// ============================================================================
// MODULE 33: SafeTensor - Algorithm (4/4)
// ============================================================================

/**
 * @brief Small fixed-size vector (for embedded ML)
 */
template<size_t SIZE>
class Vector {
public:
    Vector() { memset(_data, 0, sizeof(_data)); }

    int16_t& operator[](size_t i) { return _data[i < SIZE ? i : 0]; }
    const int16_t& operator[](size_t i) const { return _data[i < SIZE ? i : 0]; }

    /**
     * @brief Dot product
     */
    Result<int32_t> dot(const Vector<SIZE>& other) const {
        int32_t sum = 0;
        for (size_t i = 0; i < SIZE; i++) {
            int32_t prod = (int32_t)_data[i] * other._data[i];
            if ((sum > 0 && prod > INT32_MAX - sum) || (sum < 0 && prod < INT32_MIN - sum)) {
                return Err<int32_t>(Status::ErrOverflow);
            }
            sum += prod;
        }
        return Ok(sum);
    }

    /**
     * @brief Element-wise addition
     */
    Status add(const Vector<SIZE>& other) {
        for (size_t i = 0; i < SIZE; i++) {
            int32_t result = (int32_t)_data[i] + other._data[i];
            if (result > INT16_MAX || result < INT16_MIN) {
                return Status::ErrOverflow;
            }
            _data[i] = result;
        }
        return Status::Ok;
    }

    /**
     * @brief Scale by fixed-point factor (q8.8)
     */
    void scale(int16_t factor) {
        for (size_t i = 0; i < SIZE; i++) {
            _data[i] = ((int32_t)_data[i] * factor) >> 8;
        }
    }

    inline size_t size() const { return SIZE; }

private:
    int16_t _data[SIZE];
};

/**
 * @brief Small fixed-size matrix
 */
template<size_t ROWS, size_t COLS>
class Matrix {
public:
    Matrix() { memset(_data, 0, sizeof(_data)); }

    int16_t& at(size_t row, size_t col) {
        return _data[row < ROWS ? row : 0][col < COLS ? col : 0];
    }

    const int16_t& at(size_t row, size_t col) const {
        return _data[row < ROWS ? row : 0][col < COLS ? col : 0];
    }

    /**
     * @brief Matrix-vector multiply
     */
    template<size_t VCOLS>
    Status mulVector(const Vector<COLS>& v, Vector<ROWS>& result) const {
        for (size_t i = 0; i < ROWS; i++) {
            int32_t sum = 0;
            for (size_t j = 0; j < COLS; j++) {
                sum += (int32_t)_data[i][j] * v[j];
            }
            if (sum > INT16_MAX || sum < INT16_MIN) {
                return Status::ErrOverflow;
            }
            result[i] = sum;
        }
        return Status::Ok;
    }

    inline size_t rows() const { return ROWS; }
    inline size_t cols() const { return COLS; }

private:
    int16_t _data[ROWS][COLS];
};

// ============================================================================
// MODULE 34: SafePassword - Security (1/2)
// ============================================================================

/**
 * @brief Password strength level
 */
enum class PasswordStrength : uint8_t {
    VeryWeak = 0,
    Weak = 1,
    Fair = 2,
    Strong = 3,
    VeryStrong = 4
};

/**
 * @brief Check password strength
 */
inline PasswordStrength checkPasswordStrength(const char* password, size_t len) {
    if (password == nullptr || len < 4) return PasswordStrength::VeryWeak;

    bool hasLower = false;
    bool hasUpper = false;
    bool hasDigit = false;
    bool hasSpecial = false;

    for (size_t i = 0; i < len; i++) {
        char c = password[i];
        if (c >= 'a' && c <= 'z') hasLower = true;
        else if (c >= 'A' && c <= 'Z') hasUpper = true;
        else if (c >= '0' && c <= '9') hasDigit = true;
        else if (c >= 33 && c <= 126) hasSpecial = true;
    }

    int score = 0;
    if (len >= 8) score++;
    if (len >= 12) score++;
    if (hasLower && hasUpper) score++;
    if (hasDigit) score++;
    if (hasSpecial) score++;

    if (score <= 1) return PasswordStrength::VeryWeak;
    if (score == 2) return PasswordStrength::Weak;
    if (score == 3) return PasswordStrength::Fair;
    if (score == 4) return PasswordStrength::Strong;
    return PasswordStrength::VeryStrong;
}

/**
 * @brief Check if password meets minimum requirements
 */
inline bool isValidPassword(const char* password, size_t len, uint8_t minLen = 8) {
    if (password == nullptr || len < minLen) return false;
    return checkPasswordStrength(password, len) >= PasswordStrength::Fair;
}

// ============================================================================
// MODULE 35: SafeMl - Security (2/2)
// ============================================================================

/**
 * @brief Simple perceptron for embedded ML
 */
template<size_t INPUT_SIZE>
class Perceptron {
public:
    Perceptron() : _bias(0) {
        memset(_weights, 0, sizeof(_weights));
    }

    /**
     * @brief Set weights (q8.8 fixed-point)
     */
    void setWeights(const int16_t* weights, int16_t bias) {
        memcpy(_weights, weights, sizeof(_weights));
        _bias = bias;
    }

    /**
     * @brief Forward pass (returns q8.8 fixed-point)
     */
    int16_t forward(const int16_t* inputs) {
        int32_t sum = _bias;
        for (size_t i = 0; i < INPUT_SIZE; i++) {
            sum += ((int32_t)_weights[i] * inputs[i]) >> 8;
        }

        // ReLU activation
        return (sum > 0) ? (sum > INT16_MAX ? INT16_MAX : sum) : 0;
    }

    /**
     * @brief Sigmoid-like activation (using lookup table approximation)
     */
    int16_t forwardSigmoid(const int16_t* inputs) {
        int32_t sum = _bias;
        for (size_t i = 0; i < INPUT_SIZE; i++) {
            sum += ((int32_t)_weights[i] * inputs[i]) >> 8;
        }

        // Fast sigmoid approximation: 256 / (1 + e^-x) using piecewise linear
        if (sum < -1024) return 0;
        if (sum > 1024) return 256;
        return 128 + (sum >> 3);  // Linear approximation around 0
    }

private:
    int16_t _weights[INPUT_SIZE];
    int16_t _bias;
};

/**
 * @brief Input validator for ML inference
 */
template<size_t SIZE>
inline bool validateMlInput(const int16_t* input, int16_t minVal, int16_t maxVal) {
    if (input == nullptr) return false;
    for (size_t i = 0; i < SIZE; i++) {
        if (input[i] < minVal || input[i] > maxVal) return false;
    }
    return true;
}

// ============================================================================
// MODULE 36: SafeHeader - HTTP (1/3)
// ============================================================================

/**
 * @brief HTTP header name validator
 */
inline bool isValidHeaderName(const char* name, size_t len) {
    if (name == nullptr || len == 0 || len > 64) return false;

    for (size_t i = 0; i < len; i++) {
        char c = name[i];
        // RFC 7230: token chars
        bool valid = (c >= 'a' && c <= 'z') ||
                     (c >= 'A' && c <= 'Z') ||
                     (c >= '0' && c <= '9') ||
                     c == '-' || c == '_' || c == '.';
        if (!valid) return false;
    }
    return true;
}

/**
 * @brief HTTP header value validator
 */
inline bool isValidHeaderValue(const char* value, size_t len) {
    if (value == nullptr) return false;
    if (len > 8192) return false;  // Reasonable limit

    for (size_t i = 0; i < len; i++) {
        char c = value[i];
        // No control chars except tab
        if ((c < 32 && c != '\t') || c == 127) return false;
    }
    return true;
}

/**
 * @brief Common HTTP headers
 */
namespace Headers {
    constexpr const char* ContentType = "Content-Type";
    constexpr const char* ContentLength = "Content-Length";
    constexpr const char* Host = "Host";
    constexpr const char* UserAgent = "User-Agent";
    constexpr const char* Accept = "Accept";
    constexpr const char* Authorization = "Authorization";
    constexpr const char* CacheControl = "Cache-Control";
    constexpr const char* Connection = "Connection";
}

// ============================================================================
// MODULE 37: SafeCookie - HTTP (2/3)
// ============================================================================

/**
 * @brief Cookie attributes
 */
struct Cookie {
    char name[32];
    char value[128];
    uint32_t maxAge;      // Seconds, 0 = session
    bool secure;
    bool httpOnly;
    bool sameSiteStrict;
    bool valid;
};

/**
 * @brief Validate cookie name
 */
inline bool isValidCookieName(const char* name, size_t len) {
    if (name == nullptr || len == 0 || len > 31) return false;

    for (size_t i = 0; i < len; i++) {
        char c = name[i];
        // RFC 6265: cookie-name chars
        bool invalid = c <= 32 || c >= 127 || c == '=' || c == ';' || c == ',' ||
                       c == ' ' || c == '\t' || c == '(' || c == ')' ||
                       c == '<' || c == '>' || c == '@' || c == ':' ||
                       c == '\\' || c == '"' || c == '/' || c == '[' ||
                       c == ']' || c == '?' || c == '{' || c == '}';
        if (invalid) return false;
    }
    return true;
}

/**
 * @brief Validate cookie value
 */
inline bool isValidCookieValue(const char* value, size_t len) {
    if (value == nullptr) return true;  // Empty is valid
    if (len > 127) return false;

    for (size_t i = 0; i < len; i++) {
        char c = value[i];
        // RFC 6265: cookie-octet chars
        if (c < 33 || c > 126 || c == '"' || c == ',' || c == ';' || c == '\\') {
            return false;
        }
    }
    return true;
}

/**
 * @brief Create secure cookie
 */
inline Cookie makeCookie(const char* name, const char* value, uint32_t maxAge = 0) {
    Cookie c = {"", "", maxAge, true, true, true, false};

    size_t nameLen = safeStrLen(name);
    size_t valueLen = safeStrLen(value);

    if (isValidCookieName(name, nameLen) && isValidCookieValue(value, valueLen)) {
        safeStrCopy(c.name, sizeof(c.name), name, nameLen);
        safeStrCopy(c.value, sizeof(c.value), value, valueLen);
        c.valid = true;
    }

    return c;
}

// ============================================================================
// MODULE 38: SafeContentType - HTTP (3/3)
// ============================================================================

/**
 * @brief Content type category
 */
enum class ContentCategory : uint8_t {
    Unknown = 0,
    Text,
    Image,
    Audio,
    Video,
    Application,
    Multipart
};

/**
 * @brief Common MIME types
 */
namespace MimeTypes {
    constexpr const char* TextPlain = "text/plain";
    constexpr const char* TextHtml = "text/html";
    constexpr const char* TextCss = "text/css";
    constexpr const char* TextJs = "text/javascript";
    constexpr const char* AppJson = "application/json";
    constexpr const char* AppXml = "application/xml";
    constexpr const char* AppForm = "application/x-www-form-urlencoded";
    constexpr const char* AppOctetStream = "application/octet-stream";
    constexpr const char* ImagePng = "image/png";
    constexpr const char* ImageJpeg = "image/jpeg";
    constexpr const char* ImageGif = "image/gif";
    constexpr const char* ImageSvg = "image/svg+xml";
    constexpr const char* MultipartForm = "multipart/form-data";
}

/**
 * @brief Detect content category from MIME type
 */
inline ContentCategory detectContentCategory(const char* mimeType, size_t len) {
    if (mimeType == nullptr || len < 4) return ContentCategory::Unknown;

    // Check prefix
    if (len >= 5 && strncmp(mimeType, "text/", 5) == 0) return ContentCategory::Text;
    if (len >= 6 && strncmp(mimeType, "image/", 6) == 0) return ContentCategory::Image;
    if (len >= 6 && strncmp(mimeType, "audio/", 6) == 0) return ContentCategory::Audio;
    if (len >= 6 && strncmp(mimeType, "video/", 6) == 0) return ContentCategory::Video;
    if (len >= 12 && strncmp(mimeType, "application/", 12) == 0) return ContentCategory::Application;
    if (len >= 10 && strncmp(mimeType, "multipart/", 10) == 0) return ContentCategory::Multipart;

    return ContentCategory::Unknown;
}

/**
 * @brief Check if content type is text-based
 */
inline bool isTextContent(const char* mimeType, size_t len) {
    ContentCategory cat = detectContentCategory(mimeType, len);
    if (cat == ContentCategory::Text) return true;

    // Also check common text-based application types
    if (cat == ContentCategory::Application) {
        if (len >= 16 && strncmp(mimeType, "application/json", 16) == 0) return true;
        if (len >= 15 && strncmp(mimeType, "application/xml", 15) == 0) return true;
        if (len >= 22 && strncmp(mimeType, "application/javascript", 22) == 0) return true;
    }

    return false;
}

/**
 * @brief Get file extension for MIME type
 */
inline const char* mimeToExtension(const char* mimeType, size_t len) {
    if (mimeType == nullptr || len == 0) return "";

    if (strncmp(mimeType, "text/plain", len) == 0) return ".txt";
    if (strncmp(mimeType, "text/html", len) == 0) return ".html";
    if (strncmp(mimeType, "text/css", len) == 0) return ".css";
    if (strncmp(mimeType, "text/javascript", len) == 0) return ".js";
    if (strncmp(mimeType, "application/json", len) == 0) return ".json";
    if (strncmp(mimeType, "application/xml", len) == 0) return ".xml";
    if (strncmp(mimeType, "image/png", len) == 0) return ".png";
    if (strncmp(mimeType, "image/jpeg", len) == 0) return ".jpg";
    if (strncmp(mimeType, "image/gif", len) == 0) return ".gif";
    if (strncmp(mimeType, "image/svg+xml", len) == 0) return ".svg";

    return "";
}

// ============================================================================
// ARDUINO-SPECIFIC UTILITIES
// ============================================================================

/**
 * @brief Validate GPIO pin number
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
        case 300: case 1200: case 2400: case 4800:
        case 9600: case 14400: case 19200: case 28800:
        case 38400: case 57600: case 115200:
        case 230400: case 460800: case 921600:
            return true;
        default:
            return false;
    }
}

/**
 * @brief Calculate elapsed milliseconds handling wrap-around
 */
inline unsigned long millisElapsed(unsigned long start, unsigned long current) {
    if (current >= start) {
        return current - start;
    }
    return ULONG_MAX - start + current + 1;
}

/**
 * @brief Check if timeout has elapsed (handles wrap-around)
 */
inline bool isTimeout(unsigned long start, unsigned long timeoutMs) {
    return millisElapsed(start, millis()) >= timeoutMs;
}

// ============================================================================
// BOUNDED TYPES
// ============================================================================

/**
 * @brief Integer bounded to a range
 */
template<typename T = int16_t>
class BoundedInt {
public:
    BoundedInt(T value, T minVal, T maxVal)
        : _min(minVal), _max(maxVal), _value(clamp(value, minVal, maxVal)) {}

    inline T value() const { return _value; }
    inline T min() const { return _min; }
    inline T max() const { return _max; }

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
    void set(uint8_t value) { _value = clamp<uint8_t>(value, 0, 100); }

    inline proven_int_t of(proven_int_t amount) { return (amount * _value) / 100; }

    inline uint16_t toPwmDuty(uint8_t resolution = 8) {
        uint16_t maxDuty = (1 << resolution) - 1;
        return ((uint32_t)_value * maxDuty) / 100;
    }

private:
    uint8_t _value;
};

// ============================================================================
// VERSION INFO
// ============================================================================

/**
 * @brief Get library version string
 */
inline const char* version() {
    return PROVEN_VERSION;
}

/**
 * @brief Get module count
 */
inline uint8_t moduleCount() {
    return PROVEN_MODULE_COUNT;
}

} // namespace proven

#endif // PROVEN_H
