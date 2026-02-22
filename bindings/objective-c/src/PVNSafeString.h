// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeString.h
 * @brief Safe string operations delegated to libproven FFI.
 *
 * All escaping and validation is performed by the formally verified
 * Idris 2 core via the Zig FFI bridge. No string logic is reimplemented.
 *
 * Architecture:
 *   PVNSafeString -> PVNLibProven (dlopen) -> libproven.so -> Zig FFI -> Idris 2
 */

#ifndef PVNSafeString_h
#define PVNSafeString_h

#import <Foundation/Foundation.h>
#import "PVNLibProven.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * @brief Safe string operations that handle encoding safely.
 *
 * All methods delegate to libproven for escaping and validation.
 */
@interface PVNSafeString : NSObject

/**
 * @brief Initialize with a loaded library instance.
 * @param library A loaded PVNLibProven instance.
 * @return Initialized PVNSafeString wrapper.
 */
- (instancetype)initWithLibrary:(PVNLibProven *)library NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_UNAVAILABLE;

/**
 * @brief Check if raw byte data is valid UTF-8.
 * @param data Raw byte data to validate.
 * @param error Output error on failure.
 * @return NSNumber wrapping BOOL result, or nil on error.
 */
- (nullable NSNumber *)isValidUtf8:(NSData *)data
                             error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Escape a string for safe SQL interpolation.
 *
 * @note Prefer parameterized queries over string escaping.
 * @param value Input string to escape.
 * @param error Output error on failure.
 * @return Escaped string, or nil on error.
 */
- (nullable NSString *)escapeSql:(NSString *)value
                           error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Escape a string for safe HTML insertion (prevents XSS).
 * @param value Input string to escape.
 * @param error Output error on failure.
 * @return Escaped string, or nil on error.
 */
- (nullable NSString *)escapeHtml:(NSString *)value
                            error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Escape a string for safe JavaScript string literal insertion.
 * @param value Input string to escape.
 * @param error Output error on failure.
 * @return Escaped string, or nil on error.
 */
- (nullable NSString *)escapeJs:(NSString *)value
                          error:(NSError *_Nullable *_Nullable)error;

@end

NS_ASSUME_NONNULL_END

#endif /* PVNSafeString_h */
