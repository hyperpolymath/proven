// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNLibProven.m
 * @brief Dynamic library loading for libproven via dlopen/dlsym.
 *
 * This file loads libproven at runtime and resolves all FFI function
 * pointers. All computation is delegated to the Idris 2 formally
 * verified core -- no logic is reimplemented here.
 *
 * Architecture:
 *   Objective-C (PVN*) -> PVNLibProven (dlopen) -> libproven.so -> Zig FFI -> Idris 2
 */

#import "PVNLibProven.h"
#include <dlfcn.h>

// ============================================================================
// Error Domain and Helpers
// ============================================================================

NSString *const PVNProvenErrorDomain = @"com.hyperpolymath.proven";

NSString *_Nonnull PVNStatusDescription(PVNProvenStatus status) {
    switch (status) {
        case PVNProvenStatusOK:                  return @"OK";
        case PVNProvenStatusErrNullPointer:      return @"Null pointer";
        case PVNProvenStatusErrInvalidArgument:  return @"Invalid argument";
        case PVNProvenStatusErrOverflow:         return @"Overflow";
        case PVNProvenStatusErrUnderflow:        return @"Underflow";
        case PVNProvenStatusErrDivisionByZero:   return @"Division by zero";
        case PVNProvenStatusErrParseFailure:     return @"Parse failure";
        case PVNProvenStatusErrValidationFailed: return @"Validation failed";
        case PVNProvenStatusErrOutOfBounds:      return @"Out of bounds";
        case PVNProvenStatusErrEncodingError:    return @"Encoding error";
        case PVNProvenStatusErrAllocationFailed: return @"Allocation failed";
        case PVNProvenStatusErrNotImplemented:   return @"Not implemented";
        default:                                 return @"Unknown error";
    }
}

NSError *_Nullable PVNErrorFromStatus(PVNProvenStatus status) {
    if (status == PVNProvenStatusOK) {
        return nil;
    }
    return [NSError errorWithDomain:PVNProvenErrorDomain
                               code:status
                           userInfo:@{
                               NSLocalizedDescriptionKey: PVNStatusDescription(status)
                           }];
}

// ============================================================================
// PVNLibProven Implementation
// ============================================================================

@interface PVNLibProven ()

/** @brief Handle returned by dlopen. */
@property (nonatomic, assign) void *libraryHandle;

// -- Lifecycle function pointers --
@property (nonatomic, assign) pvn_init_fn fn_init;
@property (nonatomic, assign) pvn_deinit_fn fn_deinit;
@property (nonatomic, assign) pvn_is_initialized_fn fn_is_initialized;

// -- Memory function pointers --
@property (nonatomic, assign) pvn_free_string_fn fn_free_string;

// -- Version function pointers --
@property (nonatomic, assign) pvn_version_fn fn_version_major;
@property (nonatomic, assign) pvn_version_fn fn_version_minor;
@property (nonatomic, assign) pvn_version_fn fn_version_patch;
@property (nonatomic, assign) pvn_version_fn fn_module_count;
@property (nonatomic, assign) pvn_version_fn fn_ffi_abi_version;

// -- SafeMath function pointers --
@property (nonatomic, assign) pvn_math_binary_fn fn_math_add_checked;
@property (nonatomic, assign) pvn_math_binary_fn fn_math_sub_checked;
@property (nonatomic, assign) pvn_math_binary_fn fn_math_mul_checked;
@property (nonatomic, assign) pvn_math_binary_fn fn_math_div;
@property (nonatomic, assign) pvn_math_binary_fn fn_math_mod;
@property (nonatomic, assign) pvn_math_unary_fn fn_math_abs_safe;
@property (nonatomic, assign) pvn_math_clamp_fn fn_math_clamp;
@property (nonatomic, assign) pvn_math_pow_fn fn_math_pow_checked;

// -- SafeString function pointers --
@property (nonatomic, assign) pvn_string_validate_fn fn_string_is_valid_utf8;
@property (nonatomic, assign) pvn_string_escape_fn fn_string_escape_sql;
@property (nonatomic, assign) pvn_string_escape_fn fn_string_escape_html;
@property (nonatomic, assign) pvn_string_escape_fn fn_string_escape_js;

// -- SafePath function pointers --
@property (nonatomic, assign) pvn_path_traversal_fn fn_path_has_traversal;
@property (nonatomic, assign) pvn_path_sanitize_fn fn_path_sanitize_filename;

// -- SafeEmail function pointers --
@property (nonatomic, assign) pvn_email_validate_fn fn_email_is_valid;

// -- SafeUrl function pointers --
@property (nonatomic, assign) pvn_url_parse_fn fn_url_parse;
@property (nonatomic, assign) pvn_url_free_fn fn_url_free;

// -- SafeNetwork function pointers --
@property (nonatomic, assign) pvn_network_parse_ipv4_fn fn_network_parse_ipv4;
@property (nonatomic, assign) pvn_network_ipv4_check_fn fn_network_ipv4_is_private;
@property (nonatomic, assign) pvn_network_ipv4_check_fn fn_network_ipv4_is_loopback;

// -- SafeCrypto function pointers --
@property (nonatomic, assign) pvn_crypto_compare_fn fn_crypto_constant_time_eq;
@property (nonatomic, assign) pvn_crypto_random_fn fn_crypto_random_bytes;

// -- SafeJson function pointers --
@property (nonatomic, assign) pvn_json_validate_fn fn_json_is_valid;
@property (nonatomic, assign) pvn_json_type_fn fn_json_get_type;

// -- SafeFloat function pointers --
@property (nonatomic, assign) pvn_float_binary_fn fn_float_div;
@property (nonatomic, assign) pvn_float_check_fn fn_float_is_finite;
@property (nonatomic, assign) pvn_float_check_fn fn_float_is_nan;
@property (nonatomic, assign) pvn_float_unary_fn fn_float_sqrt;
@property (nonatomic, assign) pvn_float_unary_fn fn_float_ln;

// -- SafeHex function pointers --
@property (nonatomic, assign) pvn_hex_encode_fn fn_hex_encode;
@property (nonatomic, assign) pvn_hex_decode_fn fn_hex_decode;
@property (nonatomic, assign) pvn_hex_free_fn fn_hex_free;

@end

@implementation PVNLibProven

// ============================================================================
// Singleton
// ============================================================================

+ (nullable instancetype)shared {
    static PVNLibProven *sharedInstance = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        // Attempt standard library search paths.
        NSArray<NSString *> *searchPaths = @[
            @"libproven.so",
            @"libproven.dylib",
            @"/usr/local/lib/libproven.so",
            @"/usr/local/lib/libproven.dylib",
            @"/usr/lib/libproven.so",
            @"/usr/lib/libproven.dylib",
        ];

        for (NSString *path in searchPaths) {
            NSError *error = nil;
            PVNLibProven *instance = [PVNLibProven loadFromPath:path error:&error];
            if (instance != nil) {
                sharedInstance = instance;
                return;
            }
        }
    });
    return sharedInstance;
}

// ============================================================================
// Loading
// ============================================================================

+ (nullable instancetype)loadFromPath:(nonnull NSString *)path
                                error:(NSError *_Nullable *_Nullable)error {
    PVNLibProven *instance = [[PVNLibProven alloc] init];

    void *handle = dlopen([path UTF8String], RTLD_NOW | RTLD_LOCAL);
    if (handle == NULL) {
        if (error != NULL) {
            const char *dlError = dlerror();
            NSString *message = dlError != NULL
                ? [NSString stringWithUTF8String:dlError]
                : @"Unknown dlopen error";
            *error = [NSError errorWithDomain:PVNProvenErrorDomain
                                         code:-1000
                                     userInfo:@{
                                         NSLocalizedDescriptionKey: message
                                     }];
        }
        return nil;
    }

    instance.libraryHandle = handle;

    // Resolve all function pointers. If a critical function is missing,
    // the library is considered unusable.
    if (![instance resolveSymbols]) {
        dlclose(handle);
        instance.libraryHandle = NULL;
        if (error != NULL) {
            *error = [NSError errorWithDomain:PVNProvenErrorDomain
                                         code:-1001
                                     userInfo:@{
                                         NSLocalizedDescriptionKey:
                                             @"Failed to resolve required symbols from libproven"
                                     }];
        }
        return nil;
    }

    return instance;
}

/**
 * @brief Resolve all dlsym function pointers from the loaded library.
 * @return YES if all required symbols were found, NO otherwise.
 */
- (BOOL)resolveSymbols {
    void *h = self.libraryHandle;
    if (h == NULL) {
        return NO;
    }

    // Macro to reduce repetition.  Assigns function pointer or returns NO.
#define RESOLVE(name, type, ivar) \
    do { \
        self.ivar = (type)dlsym(h, name); \
        if (self.ivar == NULL) { return NO; } \
    } while (0)

    // Lifecycle
    RESOLVE("proven_init",           pvn_init_fn,           fn_init);
    RESOLVE("proven_deinit",         pvn_deinit_fn,         fn_deinit);
    RESOLVE("proven_is_initialized", pvn_is_initialized_fn, fn_is_initialized);

    // Memory
    RESOLVE("proven_free_string",    pvn_free_string_fn,    fn_free_string);

    // Version
    RESOLVE("proven_version_major",  pvn_version_fn,        fn_version_major);
    RESOLVE("proven_version_minor",  pvn_version_fn,        fn_version_minor);
    RESOLVE("proven_version_patch",  pvn_version_fn,        fn_version_patch);
    RESOLVE("proven_module_count",   pvn_version_fn,        fn_module_count);
    RESOLVE("proven_ffi_abi_version", pvn_version_fn,       fn_ffi_abi_version);

    // SafeMath
    RESOLVE("proven_math_add_checked", pvn_math_binary_fn,  fn_math_add_checked);
    RESOLVE("proven_math_sub_checked", pvn_math_binary_fn,  fn_math_sub_checked);
    RESOLVE("proven_math_mul_checked", pvn_math_binary_fn,  fn_math_mul_checked);
    RESOLVE("proven_math_div",         pvn_math_binary_fn,  fn_math_div);
    RESOLVE("proven_math_mod",         pvn_math_binary_fn,  fn_math_mod);
    RESOLVE("proven_math_abs_safe",    pvn_math_unary_fn,   fn_math_abs_safe);
    RESOLVE("proven_math_clamp",       pvn_math_clamp_fn,   fn_math_clamp);
    RESOLVE("proven_math_pow_checked", pvn_math_pow_fn,     fn_math_pow_checked);

    // SafeString
    RESOLVE("proven_string_is_valid_utf8", pvn_string_validate_fn, fn_string_is_valid_utf8);
    RESOLVE("proven_string_escape_sql",    pvn_string_escape_fn,   fn_string_escape_sql);
    RESOLVE("proven_string_escape_html",   pvn_string_escape_fn,   fn_string_escape_html);
    RESOLVE("proven_string_escape_js",     pvn_string_escape_fn,   fn_string_escape_js);

    // SafePath
    RESOLVE("proven_path_has_traversal",     pvn_path_traversal_fn, fn_path_has_traversal);
    RESOLVE("proven_path_sanitize_filename", pvn_path_sanitize_fn,  fn_path_sanitize_filename);

    // SafeEmail
    RESOLVE("proven_email_is_valid", pvn_email_validate_fn, fn_email_is_valid);

    // SafeUrl
    RESOLVE("proven_url_parse", pvn_url_parse_fn, fn_url_parse);
    RESOLVE("proven_url_free",  pvn_url_free_fn,  fn_url_free);

    // SafeNetwork
    RESOLVE("proven_network_parse_ipv4",     pvn_network_parse_ipv4_fn, fn_network_parse_ipv4);
    RESOLVE("proven_network_ipv4_is_private",  pvn_network_ipv4_check_fn, fn_network_ipv4_is_private);
    RESOLVE("proven_network_ipv4_is_loopback", pvn_network_ipv4_check_fn, fn_network_ipv4_is_loopback);

    // SafeCrypto
    RESOLVE("proven_crypto_constant_time_eq", pvn_crypto_compare_fn, fn_crypto_constant_time_eq);
    RESOLVE("proven_crypto_random_bytes",     pvn_crypto_random_fn,  fn_crypto_random_bytes);

    // SafeJson
    RESOLVE("proven_json_is_valid",  pvn_json_validate_fn, fn_json_is_valid);
    RESOLVE("proven_json_get_type",  pvn_json_type_fn,     fn_json_get_type);

    // SafeFloat
    RESOLVE("proven_float_div",       pvn_float_binary_fn, fn_float_div);
    RESOLVE("proven_float_is_finite", pvn_float_check_fn,  fn_float_is_finite);
    RESOLVE("proven_float_is_nan",    pvn_float_check_fn,  fn_float_is_nan);
    RESOLVE("proven_float_sqrt",      pvn_float_unary_fn,  fn_float_sqrt);
    RESOLVE("proven_float_ln",        pvn_float_unary_fn,  fn_float_ln);

    // SafeHex
    RESOLVE("proven_hex_encode", pvn_hex_encode_fn, fn_hex_encode);
    RESOLVE("proven_hex_decode", pvn_hex_decode_fn, fn_hex_decode);
    RESOLVE("proven_hex_free",   pvn_hex_free_fn,   fn_hex_free);

#undef RESOLVE

    return YES;
}

- (BOOL)isLoaded {
    return self.libraryHandle != NULL;
}

- (void)dealloc {
    if (self.libraryHandle != NULL) {
        dlclose(self.libraryHandle);
        self.libraryHandle = NULL;
    }
}

// ============================================================================
// Lifecycle Wrappers
// ============================================================================

- (int32_t)provenInit {
    return self.fn_init();
}

- (void)provenDeinit {
    self.fn_deinit();
}

- (BOOL)provenIsInitialized {
    return self.fn_is_initialized() ? YES : NO;
}

// ============================================================================
// Memory Wrappers
// ============================================================================

- (void)provenFreeString:(nullable char *)ptr {
    if (ptr != NULL) {
        self.fn_free_string(ptr);
    }
}

// ============================================================================
// Version Wrappers
// ============================================================================

- (uint32_t)provenVersionMajor {
    return self.fn_version_major();
}

- (uint32_t)provenVersionMinor {
    return self.fn_version_minor();
}

- (uint32_t)provenVersionPatch {
    return self.fn_version_patch();
}

- (uint32_t)provenModuleCount {
    return self.fn_module_count();
}

- (uint32_t)provenFFIABIVersion {
    return self.fn_ffi_abi_version();
}

// ============================================================================
// SafeMath Wrappers
// ============================================================================

- (PVNIntResult)provenMathAddChecked:(int64_t)a b:(int64_t)b {
    return self.fn_math_add_checked(a, b);
}

- (PVNIntResult)provenMathSubChecked:(int64_t)a b:(int64_t)b {
    return self.fn_math_sub_checked(a, b);
}

- (PVNIntResult)provenMathMulChecked:(int64_t)a b:(int64_t)b {
    return self.fn_math_mul_checked(a, b);
}

- (PVNIntResult)provenMathDiv:(int64_t)numerator denominator:(int64_t)denominator {
    return self.fn_math_div(numerator, denominator);
}

- (PVNIntResult)provenMathMod:(int64_t)numerator denominator:(int64_t)denominator {
    return self.fn_math_mod(numerator, denominator);
}

- (PVNIntResult)provenMathAbsSafe:(int64_t)n {
    return self.fn_math_abs_safe(n);
}

- (int64_t)provenMathClamp:(int64_t)lo hi:(int64_t)hi value:(int64_t)value {
    return self.fn_math_clamp(lo, hi, value);
}

- (PVNIntResult)provenMathPowChecked:(int64_t)base exp:(uint32_t)exp {
    return self.fn_math_pow_checked(base, exp);
}

// ============================================================================
// SafeString Wrappers
// ============================================================================

- (PVNBoolResult)provenStringIsValidUtf8:(nonnull const uint8_t *)ptr length:(size_t)len {
    return self.fn_string_is_valid_utf8(ptr, len);
}

- (PVNStringResult)provenStringEscapeSql:(nonnull const uint8_t *)ptr length:(size_t)len {
    return self.fn_string_escape_sql(ptr, len);
}

- (PVNStringResult)provenStringEscapeHtml:(nonnull const uint8_t *)ptr length:(size_t)len {
    return self.fn_string_escape_html(ptr, len);
}

- (PVNStringResult)provenStringEscapeJs:(nonnull const uint8_t *)ptr length:(size_t)len {
    return self.fn_string_escape_js(ptr, len);
}

// ============================================================================
// SafePath Wrappers
// ============================================================================

- (PVNBoolResult)provenPathHasTraversal:(nonnull const uint8_t *)ptr length:(size_t)len {
    return self.fn_path_has_traversal(ptr, len);
}

- (PVNStringResult)provenPathSanitizeFilename:(nonnull const uint8_t *)ptr length:(size_t)len {
    return self.fn_path_sanitize_filename(ptr, len);
}

// ============================================================================
// SafeEmail Wrappers
// ============================================================================

- (PVNBoolResult)provenEmailIsValid:(nonnull const uint8_t *)ptr length:(size_t)len {
    return self.fn_email_is_valid(ptr, len);
}

// ============================================================================
// SafeUrl Wrappers
// ============================================================================

- (PVNUrlResult)provenUrlParse:(nonnull const uint8_t *)ptr length:(size_t)len {
    return self.fn_url_parse(ptr, len);
}

- (void)provenUrlFree:(nonnull PVNUrlComponents *)components {
    self.fn_url_free(components);
}

// ============================================================================
// SafeNetwork Wrappers
// ============================================================================

- (PVNIPv4Result)provenNetworkParseIPv4:(nonnull const uint8_t *)ptr length:(size_t)len {
    return self.fn_network_parse_ipv4(ptr, len);
}

- (BOOL)provenNetworkIPv4IsPrivate:(PVNIPv4Address)addr {
    return self.fn_network_ipv4_is_private(addr) ? YES : NO;
}

- (BOOL)provenNetworkIPv4IsLoopback:(PVNIPv4Address)addr {
    return self.fn_network_ipv4_is_loopback(addr) ? YES : NO;
}

// ============================================================================
// SafeCrypto Wrappers
// ============================================================================

- (PVNBoolResult)provenCryptoConstantTimeEq:(nonnull const uint8_t *)ptr1
                                     length1:(size_t)len1
                                        ptr2:(nonnull const uint8_t *)ptr2
                                     length2:(size_t)len2 {
    return self.fn_crypto_constant_time_eq(ptr1, len1, ptr2, len2);
}

- (PVNProvenStatus)provenCryptoRandomBytes:(nonnull uint8_t *)ptr length:(size_t)len {
    return self.fn_crypto_random_bytes(ptr, len);
}

// ============================================================================
// SafeJson Wrappers
// ============================================================================

- (PVNBoolResult)provenJsonIsValid:(nonnull const uint8_t *)ptr length:(size_t)len {
    return self.fn_json_is_valid(ptr, len);
}

- (PVNJsonType)provenJsonGetType:(nonnull const uint8_t *)ptr length:(size_t)len {
    return self.fn_json_get_type(ptr, len);
}

// ============================================================================
// SafeFloat Wrappers
// ============================================================================

- (PVNFloatResult)provenFloatDiv:(double)a b:(double)b {
    return self.fn_float_div(a, b);
}

- (BOOL)provenFloatIsFinite:(double)x {
    return self.fn_float_is_finite(x) ? YES : NO;
}

- (BOOL)provenFloatIsNaN:(double)x {
    return self.fn_float_is_nan(x) ? YES : NO;
}

- (PVNFloatResult)provenFloatSqrt:(double)x {
    return self.fn_float_sqrt(x);
}

- (PVNFloatResult)provenFloatLn:(double)x {
    return self.fn_float_ln(x);
}

// ============================================================================
// SafeHex Wrappers
// ============================================================================

- (PVNStringResult)provenHexEncode:(nonnull const uint8_t *)ptr
                            length:(size_t)len
                         uppercase:(BOOL)uppercase {
    return self.fn_hex_encode(ptr, len, uppercase ? true : false);
}

- (PVNHexDecodeResult)provenHexDecode:(nonnull const uint8_t *)ptr length:(size_t)len {
    return self.fn_hex_decode(ptr, len);
}

- (void)provenHexFree:(nonnull PVNHexDecodeResult *)result {
    self.fn_hex_free(result);
}

@end
