/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_uuid.c
 * @brief Safe UUID generation and parsing implementation.
 */

#include "../include/safe_uuid.h"
#include <string.h>
#include <stdio.h>
#include <ctype.h>

/* Platform-specific random bytes */
#ifdef _WIN32
#include <windows.h>
#include <bcrypt.h>
#pragma comment(lib, "bcrypt.lib")
#else
#include <fcntl.h>
#include <unistd.h>
#endif

/* ============================================================================
 * Well-Known UUIDs
 * ============================================================================ */

const Uuid UUID_NIL = { .bytes = {0} };

const Uuid UUID_NAMESPACE_DNS = {
    .bytes = {
        0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
    }
};

const Uuid UUID_NAMESPACE_URL = {
    .bytes = {
        0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
    }
};

const Uuid UUID_NAMESPACE_OID = {
    .bytes = {
        0x6b, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
    }
};

const Uuid UUID_NAMESPACE_X500 = {
    .bytes = {
        0x6b, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
    }
};

/* ============================================================================
 * Internal Helpers
 * ============================================================================ */

static const char HEX_LOWER[] = "0123456789abcdef";
static const char HEX_UPPER[] = "0123456789ABCDEF";

/**
 * @brief Convert hex character to nibble value
 * @param c Hex character
 * @param[out] nibble Output nibble value
 * @return true if valid hex char, false otherwise
 */
static bool hex_char_to_nibble(char c, uint8_t* nibble) {
    if (c >= '0' && c <= '9') {
        *nibble = (uint8_t)(c - '0');
        return true;
    }
    if (c >= 'a' && c <= 'f') {
        *nibble = (uint8_t)(c - 'a' + 10);
        return true;
    }
    if (c >= 'A' && c <= 'F') {
        *nibble = (uint8_t)(c - 'A' + 10);
        return true;
    }
    return false;
}

/**
 * @brief Get cryptographically secure random bytes
 * @param buf Buffer to fill
 * @param len Number of bytes to generate
 * @return true on success, false on failure
 */
static bool get_random_bytes(uint8_t* buf, size_t len) {
#ifdef _WIN32
    NTSTATUS status = BCryptGenRandom(NULL, buf, (ULONG)len, BCRYPT_USE_SYSTEM_PREFERRED_RNG);
    return BCRYPT_SUCCESS(status);
#else
    int fd = open("/dev/urandom", O_RDONLY);
    if (fd < 0) {
        return false;
    }
    ssize_t bytes_read = read(fd, buf, len);
    close(fd);
    return bytes_read == (ssize_t)len;
#endif
}

/* ============================================================================
 * UUID Generation
 * ============================================================================ */

UuidStatus uuid_v4_generate(Uuid* uuid) {
    if (uuid == NULL) {
        return UUID_ERR_NULL_POINTER;
    }

    uint8_t random_bytes[UUID_BYTES_LEN];
    if (!get_random_bytes(random_bytes, UUID_BYTES_LEN)) {
        return UUID_ERR_RANDOM_FAILED;
    }

    return uuid_v4_from_bytes(random_bytes, uuid);
}

UuidStatus uuid_v4_from_bytes(const uint8_t random_bytes[UUID_BYTES_LEN], Uuid* uuid) {
    if (random_bytes == NULL || uuid == NULL) {
        return UUID_ERR_NULL_POINTER;
    }

    /* Copy random bytes */
    memcpy(uuid->bytes, random_bytes, UUID_BYTES_LEN);

    /* Set version to 4 (random) */
    uuid->bytes[6] = (uuid->bytes[6] & 0x0F) | 0x40;

    /* Set variant to RFC 4122 */
    uuid->bytes[8] = (uuid->bytes[8] & 0x3F) | 0x80;

    return UUID_OK;
}

/* ============================================================================
 * UUID Parsing
 * ============================================================================ */

UuidStatus uuid_parse(const char* str, size_t len, Uuid* uuid) {
    if (str == NULL || uuid == NULL) {
        return UUID_ERR_NULL_POINTER;
    }

    if (len != UUID_STRING_LEN) {
        return UUID_ERR_INVALID_LENGTH;
    }

    /* Validate format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx */
    if (str[8] != '-' || str[13] != '-' || str[18] != '-' || str[23] != '-') {
        return UUID_ERR_INVALID_FORMAT;
    }

    /* Parse hex digits */
    static const size_t positions[] = {
        0, 2, 4, 6,     /* 8 hex chars */
        9, 11,          /* 4 hex chars (skip dash at 8) */
        14, 16,         /* 4 hex chars (skip dash at 13) */
        19, 21,         /* 4 hex chars (skip dash at 18) */
        24, 26, 28, 30, 32, 34 /* 12 hex chars (skip dash at 23) */
    };

    for (size_t i = 0; i < UUID_BYTES_LEN; i++) {
        uint8_t high_nibble, low_nibble;
        size_t pos = positions[i];

        if (!hex_char_to_nibble(str[pos], &high_nibble) ||
            !hex_char_to_nibble(str[pos + 1], &low_nibble)) {
            return UUID_ERR_INVALID_HEX;
        }

        uuid->bytes[i] = (high_nibble << 4) | low_nibble;
    }

    return UUID_OK;
}

UuidStatus uuid_parse_cstr(const char* str, Uuid* uuid) {
    if (str == NULL) {
        return UUID_ERR_NULL_POINTER;
    }
    return uuid_parse(str, strlen(str), uuid);
}

UuidStatus uuid_from_bytes(const uint8_t bytes[UUID_BYTES_LEN], Uuid* uuid) {
    if (bytes == NULL || uuid == NULL) {
        return UUID_ERR_NULL_POINTER;
    }
    memcpy(uuid->bytes, bytes, UUID_BYTES_LEN);
    return UUID_OK;
}

/* ============================================================================
 * UUID Formatting
 * ============================================================================ */

UuidStatus uuid_to_string(const Uuid* uuid, char* buf, size_t buf_size) {
    if (uuid == NULL || buf == NULL) {
        return UUID_ERR_NULL_POINTER;
    }

    if (buf_size < UUID_STRING_LEN + 1) {
        return UUID_ERR_BUFFER_TOO_SMALL;
    }

    const uint8_t* b = uuid->bytes;
    snprintf(buf, buf_size,
             "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
             b[0], b[1], b[2], b[3],
             b[4], b[5],
             b[6], b[7],
             b[8], b[9],
             b[10], b[11], b[12], b[13], b[14], b[15]);

    return UUID_OK;
}

UuidStatus uuid_to_urn(const Uuid* uuid, char* buf, size_t buf_size) {
    if (uuid == NULL || buf == NULL) {
        return UUID_ERR_NULL_POINTER;
    }

    if (buf_size < UUID_URN_LEN + 1) {
        return UUID_ERR_BUFFER_TOO_SMALL;
    }

    /* Write "urn:uuid:" prefix */
    memcpy(buf, "urn:uuid:", 9);

    /* Format the UUID part */
    char uuid_str[UUID_STRING_LEN + 1];
    UuidStatus status = uuid_to_string(uuid, uuid_str, sizeof(uuid_str));
    if (status != UUID_OK) {
        return status;
    }

    memcpy(buf + 9, uuid_str, UUID_STRING_LEN);
    buf[UUID_URN_LEN] = '\0';

    return UUID_OK;
}

UuidStatus uuid_to_string_upper(const Uuid* uuid, char* buf, size_t buf_size) {
    if (uuid == NULL || buf == NULL) {
        return UUID_ERR_NULL_POINTER;
    }

    if (buf_size < UUID_STRING_LEN + 1) {
        return UUID_ERR_BUFFER_TOO_SMALL;
    }

    const uint8_t* b = uuid->bytes;
    snprintf(buf, buf_size,
             "%02X%02X%02X%02X-%02X%02X-%02X%02X-%02X%02X-%02X%02X%02X%02X%02X%02X",
             b[0], b[1], b[2], b[3],
             b[4], b[5],
             b[6], b[7],
             b[8], b[9],
             b[10], b[11], b[12], b[13], b[14], b[15]);

    return UUID_OK;
}

/* ============================================================================
 * UUID Queries
 * ============================================================================ */

UuidVersionResult uuid_get_version(const Uuid* uuid) {
    UuidVersionResult result = { .status = UUID_OK, .version = UUID_VERSION_NIL };

    if (uuid == NULL) {
        result.status = UUID_ERR_NULL_POINTER;
        return result;
    }

    uint8_t version_bits = (uuid->bytes[6] >> 4) & 0x0F;
    switch (version_bits) {
        case 1: result.version = UUID_VERSION_1; break;
        case 2: result.version = UUID_VERSION_2; break;
        case 3: result.version = UUID_VERSION_3; break;
        case 4: result.version = UUID_VERSION_4; break;
        case 5: result.version = UUID_VERSION_5; break;
        default: result.version = UUID_VERSION_NIL; break;
    }

    return result;
}

UuidVariantResult uuid_get_variant(const Uuid* uuid) {
    UuidVariantResult result = { .status = UUID_OK, .variant = UUID_VARIANT_NCS };

    if (uuid == NULL) {
        result.status = UUID_ERR_NULL_POINTER;
        return result;
    }

    uint8_t variant_byte = uuid->bytes[8];

    if ((variant_byte >> 7) == 0) {
        result.variant = UUID_VARIANT_NCS;
    } else if ((variant_byte >> 6) == 0b10) {
        result.variant = UUID_VARIANT_RFC4122;
    } else if ((variant_byte >> 5) == 0b110) {
        result.variant = UUID_VARIANT_MICROSOFT;
    } else {
        result.variant = UUID_VARIANT_FUTURE;
    }

    return result;
}

bool uuid_is_nil(const Uuid* uuid) {
    if (uuid == NULL) {
        return false;
    }

    for (size_t i = 0; i < UUID_BYTES_LEN; i++) {
        if (uuid->bytes[i] != 0) {
            return false;
        }
    }
    return true;
}

int uuid_compare(const Uuid* a, const Uuid* b) {
    if (a == NULL && b == NULL) return 0;
    if (a == NULL) return -1;
    if (b == NULL) return 1;

    return memcmp(a->bytes, b->bytes, UUID_BYTES_LEN);
}

bool uuid_equals(const Uuid* a, const Uuid* b) {
    return uuid_compare(a, b) == 0;
}

/* ============================================================================
 * UUID Validation
 * ============================================================================ */

bool uuid_is_valid(const char* str, size_t len) {
    if (str == NULL || len != UUID_STRING_LEN) {
        return false;
    }

    /* Check dash positions */
    if (str[8] != '-' || str[13] != '-' || str[18] != '-' || str[23] != '-') {
        return false;
    }

    /* Check all other characters are hex */
    for (size_t i = 0; i < len; i++) {
        if (i == 8 || i == 13 || i == 18 || i == 23) {
            continue; /* Skip dashes */
        }
        if (!isxdigit((unsigned char)str[i])) {
            return false;
        }
    }

    return true;
}

bool uuid_is_valid_cstr(const char* str) {
    if (str == NULL) {
        return false;
    }
    return uuid_is_valid(str, strlen(str));
}

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

const char* uuid_status_string(UuidStatus status) {
    switch (status) {
        case UUID_OK:                 return "Success";
        case UUID_ERR_NULL_POINTER:   return "Null pointer";
        case UUID_ERR_INVALID_FORMAT: return "Invalid format";
        case UUID_ERR_INVALID_LENGTH: return "Invalid length";
        case UUID_ERR_INVALID_HEX:    return "Invalid hex character";
        case UUID_ERR_BUFFER_TOO_SMALL: return "Buffer too small";
        case UUID_ERR_RANDOM_FAILED:  return "Random generation failed";
        default:                      return "Unknown error";
    }
}

const char* uuid_version_string(UuidVersion version) {
    switch (version) {
        case UUID_VERSION_NIL: return "Nil";
        case UUID_VERSION_1:   return "Time-based (v1)";
        case UUID_VERSION_2:   return "DCE Security (v2)";
        case UUID_VERSION_3:   return "Name-based MD5 (v3)";
        case UUID_VERSION_4:   return "Random (v4)";
        case UUID_VERSION_5:   return "Name-based SHA-1 (v5)";
        default:               return "Unknown";
    }
}

const char* uuid_variant_string(UuidVariant variant) {
    switch (variant) {
        case UUID_VARIANT_NCS:       return "NCS";
        case UUID_VARIANT_RFC4122:   return "RFC 4122";
        case UUID_VARIANT_MICROSOFT: return "Microsoft";
        case UUID_VARIANT_FUTURE:    return "Future";
        default:                     return "Unknown";
    }
}
