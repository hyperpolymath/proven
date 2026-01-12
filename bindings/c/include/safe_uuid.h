/* SPDX-License-Identifier: AGPL-3.0-or-later */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_uuid.h
 * @brief Safe UUID generation and parsing following RFC 4122.
 *
 * This module provides UUID v4 generation, parsing from canonical format,
 * and formatting (canonical and URN). All operations handle errors gracefully.
 */

#ifndef SAFE_UUID_H
#define SAFE_UUID_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

/** Length of UUID in bytes */
#define UUID_BYTES_LEN 16

/** Length of canonical UUID string (e.g., "550e8400-e29b-41d4-a716-446655440000") */
#define UUID_STRING_LEN 36

/** Length of URN format (e.g., "urn:uuid:550e8400-e29b-41d4-a716-446655440000") */
#define UUID_URN_LEN 45

/* ============================================================================
 * Status Codes
 * ============================================================================ */

/**
 * @brief Status codes for UUID operations
 */
typedef enum UuidStatus {
    UUID_OK = 0,
    UUID_ERR_NULL_POINTER = -1,
    UUID_ERR_INVALID_FORMAT = -2,
    UUID_ERR_INVALID_LENGTH = -3,
    UUID_ERR_INVALID_HEX = -4,
    UUID_ERR_BUFFER_TOO_SMALL = -5,
    UUID_ERR_RANDOM_FAILED = -6
} UuidStatus;

/* ============================================================================
 * Enumerations
 * ============================================================================ */

/**
 * @brief UUID version types per RFC 4122
 */
typedef enum UuidVersion {
    UUID_VERSION_NIL = 0,     /**< Nil UUID */
    UUID_VERSION_1 = 1,       /**< Time-based */
    UUID_VERSION_2 = 2,       /**< DCE Security */
    UUID_VERSION_3 = 3,       /**< Name-based (MD5) */
    UUID_VERSION_4 = 4,       /**< Random */
    UUID_VERSION_5 = 5        /**< Name-based (SHA-1) */
} UuidVersion;

/**
 * @brief UUID variant types per RFC 4122
 */
typedef enum UuidVariant {
    UUID_VARIANT_NCS = 0,      /**< Reserved, NCS backward compatibility */
    UUID_VARIANT_RFC4122 = 1,  /**< RFC 4122 (standard) */
    UUID_VARIANT_MICROSOFT = 2,/**< Reserved, Microsoft backward compatibility */
    UUID_VARIANT_FUTURE = 3    /**< Reserved for future definition */
} UuidVariant;

/* ============================================================================
 * Structures
 * ============================================================================ */

/**
 * @brief UUID structure (128-bit identifier)
 */
typedef struct Uuid {
    uint8_t bytes[UUID_BYTES_LEN];
} Uuid;

/**
 * @brief Result type for UUID parsing
 */
typedef struct UuidResult {
    UuidStatus status;
    Uuid uuid;
} UuidResult;

/**
 * @brief Result type for version query
 */
typedef struct UuidVersionResult {
    UuidStatus status;
    UuidVersion version;
} UuidVersionResult;

/**
 * @brief Result type for variant query
 */
typedef struct UuidVariantResult {
    UuidStatus status;
    UuidVariant variant;
} UuidVariantResult;

/* ============================================================================
 * Well-Known UUIDs
 * ============================================================================ */

/**
 * @brief The nil UUID (all zeros)
 */
extern const Uuid UUID_NIL;

/**
 * @brief DNS namespace UUID for name-based generation
 */
extern const Uuid UUID_NAMESPACE_DNS;

/**
 * @brief URL namespace UUID for name-based generation
 */
extern const Uuid UUID_NAMESPACE_URL;

/**
 * @brief OID namespace UUID for name-based generation
 */
extern const Uuid UUID_NAMESPACE_OID;

/**
 * @brief X.500 namespace UUID for name-based generation
 */
extern const Uuid UUID_NAMESPACE_X500;

/* ============================================================================
 * UUID Generation
 * ============================================================================ */

/**
 * @brief Generate a v4 (random) UUID
 * @param[out] uuid Pointer to store the generated UUID
 * @return UUID_OK on success, error code otherwise
 *
 * Uses cryptographically secure random bytes from the system.
 */
UuidStatus uuid_v4_generate(Uuid* uuid);

/**
 * @brief Create a v4 UUID from provided random bytes
 * @param random_bytes 16 bytes of random data
 * @param[out] uuid Pointer to store the generated UUID
 * @return UUID_OK on success, error code otherwise
 *
 * Sets version and variant bits appropriately.
 */
UuidStatus uuid_v4_from_bytes(const uint8_t random_bytes[UUID_BYTES_LEN], Uuid* uuid);

/* ============================================================================
 * UUID Parsing
 * ============================================================================ */

/**
 * @brief Parse UUID from canonical string format
 * @param str UUID string (e.g., "550e8400-e29b-41d4-a716-446655440000")
 * @param len Length of the string
 * @param[out] uuid Pointer to store the parsed UUID
 * @return UUID_OK on success, error code otherwise
 */
UuidStatus uuid_parse(const char* str, size_t len, Uuid* uuid);

/**
 * @brief Parse UUID from canonical string (null-terminated)
 * @param str Null-terminated UUID string
 * @param[out] uuid Pointer to store the parsed UUID
 * @return UUID_OK on success, error code otherwise
 */
UuidStatus uuid_parse_cstr(const char* str, Uuid* uuid);

/**
 * @brief Create UUID from raw bytes
 * @param bytes 16 bytes of UUID data
 * @param[out] uuid Pointer to store the UUID
 * @return UUID_OK on success, error code otherwise
 */
UuidStatus uuid_from_bytes(const uint8_t bytes[UUID_BYTES_LEN], Uuid* uuid);

/* ============================================================================
 * UUID Formatting
 * ============================================================================ */

/**
 * @brief Format UUID as canonical string
 * @param uuid Pointer to the UUID
 * @param[out] buf Buffer for the output string
 * @param buf_size Size of the buffer (must be >= UUID_STRING_LEN + 1)
 * @return UUID_OK on success, error code otherwise
 *
 * Output format: "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx" (lowercase)
 */
UuidStatus uuid_to_string(const Uuid* uuid, char* buf, size_t buf_size);

/**
 * @brief Format UUID as URN
 * @param uuid Pointer to the UUID
 * @param[out] buf Buffer for the output string
 * @param buf_size Size of the buffer (must be >= UUID_URN_LEN + 1)
 * @return UUID_OK on success, error code otherwise
 *
 * Output format: "urn:uuid:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
 */
UuidStatus uuid_to_urn(const Uuid* uuid, char* buf, size_t buf_size);

/**
 * @brief Format UUID as uppercase canonical string
 * @param uuid Pointer to the UUID
 * @param[out] buf Buffer for the output string
 * @param buf_size Size of the buffer (must be >= UUID_STRING_LEN + 1)
 * @return UUID_OK on success, error code otherwise
 */
UuidStatus uuid_to_string_upper(const Uuid* uuid, char* buf, size_t buf_size);

/* ============================================================================
 * UUID Queries
 * ============================================================================ */

/**
 * @brief Get UUID version
 * @param uuid Pointer to the UUID
 * @return UuidVersionResult with status and version
 */
UuidVersionResult uuid_get_version(const Uuid* uuid);

/**
 * @brief Get UUID variant
 * @param uuid Pointer to the UUID
 * @return UuidVariantResult with status and variant
 */
UuidVariantResult uuid_get_variant(const Uuid* uuid);

/**
 * @brief Check if UUID is nil (all zeros)
 * @param uuid Pointer to the UUID
 * @return true if nil, false otherwise
 */
bool uuid_is_nil(const Uuid* uuid);

/**
 * @brief Compare two UUIDs
 * @param a First UUID
 * @param b Second UUID
 * @return 0 if equal, negative if a < b, positive if a > b
 */
int uuid_compare(const Uuid* a, const Uuid* b);

/**
 * @brief Check if two UUIDs are equal
 * @param a First UUID
 * @param b Second UUID
 * @return true if equal, false otherwise
 */
bool uuid_equals(const Uuid* a, const Uuid* b);

/* ============================================================================
 * UUID Validation
 * ============================================================================ */

/**
 * @brief Check if string is valid UUID format
 * @param str String to validate
 * @param len Length of the string
 * @return true if valid UUID format, false otherwise
 */
bool uuid_is_valid(const char* str, size_t len);

/**
 * @brief Check if null-terminated string is valid UUID format
 * @param str Null-terminated string to validate
 * @return true if valid UUID format, false otherwise
 */
bool uuid_is_valid_cstr(const char* str);

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

/**
 * @brief Get status code description
 * @param status Status code
 * @return Static string describing the status
 */
const char* uuid_status_string(UuidStatus status);

/**
 * @brief Get version name
 * @param version UUID version
 * @return Static string with version name
 */
const char* uuid_version_string(UuidVersion version);

/**
 * @brief Get variant name
 * @param variant UUID variant
 * @return Static string with variant name
 */
const char* uuid_variant_string(UuidVariant variant);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_UUID_H */
