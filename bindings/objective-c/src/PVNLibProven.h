// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNLibProven.h
 * @brief C function declarations for libproven dynamic library.
 *
 * This header declares all extern C functions exported by libproven.
 * These are loaded at runtime via dlopen/dlsym in PVNLibProven.m.
 *
 * All computation is performed by the formally verified Idris 2 core
 * via the Zig FFI bridge. This header is a pure declaration layer.
 * Do NOT reimplement any logic in Objective-C.
 *
 * Architecture:
 *   Objective-C (PVN*) -> PVNLibProven (dlopen) -> libproven.so -> Zig FFI -> Idris 2
 */

#ifndef PVNLibProven_h
#define PVNLibProven_h

#import <Foundation/Foundation.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

// ============================================================================
// Status Codes
// ============================================================================

/**
 * @brief Status codes returned by Proven operations.
 *
 * Zero indicates success; negative values indicate specific error conditions.
 */
typedef NS_ENUM(int32_t, PVNProvenStatus) {
    PVNProvenStatusOK                   =   0,
    PVNProvenStatusErrNullPointer       =  -1,
    PVNProvenStatusErrInvalidArgument   =  -2,
    PVNProvenStatusErrOverflow          =  -3,
    PVNProvenStatusErrUnderflow         =  -4,
    PVNProvenStatusErrDivisionByZero    =  -5,
    PVNProvenStatusErrParseFailure      =  -6,
    PVNProvenStatusErrValidationFailed  =  -7,
    PVNProvenStatusErrOutOfBounds       =  -8,
    PVNProvenStatusErrEncodingError     =  -9,
    PVNProvenStatusErrAllocationFailed  = -10,
    PVNProvenStatusErrNotImplemented    = -99
};

// ============================================================================
// Core Result Types (C structs matching libproven ABI)
// ============================================================================

/** @brief Result for integer operations. */
typedef struct {
    int32_t status;
    int64_t value;
} PVNIntResult;

/** @brief Result for boolean operations. */
typedef struct {
    int32_t status;
    bool    value;
} PVNBoolResult;

/**
 * @brief Result for string operations.
 * @note Caller must free value using pvn_proven_free_string().
 */
typedef struct {
    int32_t status;
    char   *value;
    size_t  length;
} PVNStringResult;

/** @brief Result for floating-point operations. */
typedef struct {
    int32_t status;
    double  value;
} PVNFloatResult;

/** @brief Parsed URL components from libproven. */
typedef struct {
    char    *scheme;        size_t scheme_len;
    char    *host;          size_t host_len;
    uint16_t port;          bool   has_port;
    char    *path;          size_t path_len;
    char    *query;         size_t query_len;
    char    *fragment;      size_t fragment_len;
} PVNUrlComponents;

/** @brief Result for URL parsing. */
typedef struct {
    int32_t          status;
    PVNUrlComponents components;
} PVNUrlResult;

/** @brief IPv4 address structure. */
typedef struct {
    uint8_t octets[4];
} PVNIPv4Address;

/** @brief Result for IPv4 parsing. */
typedef struct {
    int32_t        status;
    PVNIPv4Address address;
} PVNIPv4Result;

/** @brief JSON value type enumeration. */
typedef NS_ENUM(int32_t, PVNJsonType) {
    PVNJsonTypeNull    =  0,
    PVNJsonTypeBool    =  1,
    PVNJsonTypeNumber  =  2,
    PVNJsonTypeString  =  3,
    PVNJsonTypeArray   =  4,
    PVNJsonTypeObject  =  5,
    PVNJsonTypeInvalid = -1
};

/** @brief Hex decode result with byte data. */
typedef struct {
    int32_t  status;
    uint8_t *data;
    size_t   length;
} PVNHexDecodeResult;

// ============================================================================
// C Function Pointer Types (for dlsym resolution)
// ============================================================================

// Lifecycle
typedef int32_t (*pvn_init_fn)(void);
typedef void    (*pvn_deinit_fn)(void);
typedef bool    (*pvn_is_initialized_fn)(void);

// Memory
typedef void    (*pvn_free_string_fn)(char *ptr);

// Version
typedef uint32_t (*pvn_version_fn)(void);

// SafeMath
typedef PVNIntResult (*pvn_math_binary_fn)(int64_t a, int64_t b);
typedef PVNIntResult (*pvn_math_unary_fn)(int64_t n);
typedef int64_t      (*pvn_math_clamp_fn)(int64_t lo, int64_t hi, int64_t value);
typedef PVNIntResult (*pvn_math_pow_fn)(int64_t base, uint32_t exp);

// SafeString
typedef PVNBoolResult   (*pvn_string_validate_fn)(const uint8_t *ptr, size_t len);
typedef PVNStringResult (*pvn_string_escape_fn)(const uint8_t *ptr, size_t len);

// SafePath
typedef PVNBoolResult   (*pvn_path_traversal_fn)(const uint8_t *ptr, size_t len);
typedef PVNStringResult (*pvn_path_sanitize_fn)(const uint8_t *ptr, size_t len);

// SafeEmail
typedef PVNBoolResult (*pvn_email_validate_fn)(const uint8_t *ptr, size_t len);

// SafeUrl
typedef PVNUrlResult (*pvn_url_parse_fn)(const uint8_t *ptr, size_t len);
typedef void         (*pvn_url_free_fn)(PVNUrlComponents *components);

// SafeNetwork
typedef PVNIPv4Result (*pvn_network_parse_ipv4_fn)(const uint8_t *ptr, size_t len);
typedef bool          (*pvn_network_ipv4_check_fn)(PVNIPv4Address addr);

// SafeCrypto
typedef PVNBoolResult    (*pvn_crypto_compare_fn)(const uint8_t *ptr1, size_t len1,
                                                   const uint8_t *ptr2, size_t len2);
typedef PVNProvenStatus  (*pvn_crypto_random_fn)(uint8_t *ptr, size_t len);

// SafeJson
typedef PVNBoolResult (*pvn_json_validate_fn)(const uint8_t *ptr, size_t len);
typedef PVNJsonType   (*pvn_json_type_fn)(const uint8_t *ptr, size_t len);

// SafeFloat
typedef PVNFloatResult (*pvn_float_binary_fn)(double a, double b);
typedef PVNFloatResult (*pvn_float_unary_fn)(double x);
typedef bool           (*pvn_float_check_fn)(double x);

// SafeHex
typedef PVNStringResult    (*pvn_hex_encode_fn)(const uint8_t *ptr, size_t len, bool uppercase);
typedef PVNHexDecodeResult (*pvn_hex_decode_fn)(const uint8_t *ptr, size_t len);
typedef void               (*pvn_hex_free_fn)(PVNHexDecodeResult *result);

// ============================================================================
// PVNLibProven - Dynamic Library Loader
// ============================================================================

/**
 * @brief Manages dynamic loading of libproven and provides access
 *        to all FFI function pointers.
 *
 * This class uses dlopen/dlsym to load libproven at runtime.
 * All function pointers are resolved during initialization.
 * Thread-safe singleton access via +[PVNLibProven shared].
 */
@interface PVNLibProven : NSObject

/** @brief Shared singleton instance. Thread-safe. */
+ (nullable instancetype)shared;

/**
 * @brief Load libproven from a specific path.
 * @param path Path to libproven.so or libproven.dylib.
 * @param error Output error if loading fails.
 * @return Loaded library instance, or nil on failure.
 */
+ (nullable instancetype)loadFromPath:(nonnull NSString *)path
                                error:(NSError *_Nullable *_Nullable)error;

/** @brief Whether the library was successfully loaded. */
@property (nonatomic, readonly) BOOL isLoaded;

// -- Lifecycle --
- (int32_t)provenInit;
- (void)provenDeinit;
- (BOOL)provenIsInitialized;

// -- Memory --
- (void)provenFreeString:(nullable char *)ptr;

// -- Version --
- (uint32_t)provenVersionMajor;
- (uint32_t)provenVersionMinor;
- (uint32_t)provenVersionPatch;
- (uint32_t)provenModuleCount;
- (uint32_t)provenFFIABIVersion;

// -- SafeMath --
- (PVNIntResult)provenMathAddChecked:(int64_t)a b:(int64_t)b;
- (PVNIntResult)provenMathSubChecked:(int64_t)a b:(int64_t)b;
- (PVNIntResult)provenMathMulChecked:(int64_t)a b:(int64_t)b;
- (PVNIntResult)provenMathDiv:(int64_t)numerator denominator:(int64_t)denominator;
- (PVNIntResult)provenMathMod:(int64_t)numerator denominator:(int64_t)denominator;
- (PVNIntResult)provenMathAbsSafe:(int64_t)n;
- (int64_t)provenMathClamp:(int64_t)lo hi:(int64_t)hi value:(int64_t)value;
- (PVNIntResult)provenMathPowChecked:(int64_t)base exp:(uint32_t)exp;

// -- SafeString --
- (PVNBoolResult)provenStringIsValidUtf8:(nonnull const uint8_t *)ptr length:(size_t)len;
- (PVNStringResult)provenStringEscapeSql:(nonnull const uint8_t *)ptr length:(size_t)len;
- (PVNStringResult)provenStringEscapeHtml:(nonnull const uint8_t *)ptr length:(size_t)len;
- (PVNStringResult)provenStringEscapeJs:(nonnull const uint8_t *)ptr length:(size_t)len;

// -- SafePath --
- (PVNBoolResult)provenPathHasTraversal:(nonnull const uint8_t *)ptr length:(size_t)len;
- (PVNStringResult)provenPathSanitizeFilename:(nonnull const uint8_t *)ptr length:(size_t)len;

// -- SafeEmail --
- (PVNBoolResult)provenEmailIsValid:(nonnull const uint8_t *)ptr length:(size_t)len;

// -- SafeUrl --
- (PVNUrlResult)provenUrlParse:(nonnull const uint8_t *)ptr length:(size_t)len;
- (void)provenUrlFree:(nonnull PVNUrlComponents *)components;

// -- SafeNetwork --
- (PVNIPv4Result)provenNetworkParseIPv4:(nonnull const uint8_t *)ptr length:(size_t)len;
- (BOOL)provenNetworkIPv4IsPrivate:(PVNIPv4Address)addr;
- (BOOL)provenNetworkIPv4IsLoopback:(PVNIPv4Address)addr;

// -- SafeCrypto --
- (PVNBoolResult)provenCryptoConstantTimeEq:(nonnull const uint8_t *)ptr1
                                     length1:(size_t)len1
                                        ptr2:(nonnull const uint8_t *)ptr2
                                     length2:(size_t)len2;
- (PVNProvenStatus)provenCryptoRandomBytes:(nonnull uint8_t *)ptr length:(size_t)len;

// -- SafeJson --
- (PVNBoolResult)provenJsonIsValid:(nonnull const uint8_t *)ptr length:(size_t)len;
- (PVNJsonType)provenJsonGetType:(nonnull const uint8_t *)ptr length:(size_t)len;

// -- SafeFloat --
- (PVNFloatResult)provenFloatDiv:(double)a b:(double)b;
- (BOOL)provenFloatIsFinite:(double)x;
- (BOOL)provenFloatIsNaN:(double)x;
- (PVNFloatResult)provenFloatSqrt:(double)x;
- (PVNFloatResult)provenFloatLn:(double)x;

// -- SafeHex --
- (PVNStringResult)provenHexEncode:(nonnull const uint8_t *)ptr
                            length:(size_t)len
                         uppercase:(BOOL)uppercase;
- (PVNHexDecodeResult)provenHexDecode:(nonnull const uint8_t *)ptr length:(size_t)len;
- (void)provenHexFree:(nonnull PVNHexDecodeResult *)result;

@end

// ============================================================================
// Error Domain
// ============================================================================

/** @brief NSError domain for Proven library errors. */
extern NSString *const _Nonnull PVNProvenErrorDomain;

/**
 * @brief Create an NSError from a PVNProvenStatus code.
 * @param status The status code from libproven.
 * @return An NSError, or nil if status is PVNProvenStatusOK.
 */
extern NSError *_Nullable PVNErrorFromStatus(PVNProvenStatus status);

/**
 * @brief Human-readable description for a status code.
 * @param status The status code from libproven.
 * @return A descriptive string.
 */
extern NSString *_Nonnull PVNStatusDescription(PVNProvenStatus status);

#endif /* PVNLibProven_h */
