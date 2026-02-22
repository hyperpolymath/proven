// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeJson.h
 * @brief Safe JSON validation delegated to libproven FFI.
 *
 * Validates JSON structure without full parsing, using the formally
 * verified Idris 2 core via the Zig FFI bridge. No JSON logic is reimplemented.
 *
 * Architecture:
 *   PVNSafeJson -> PVNLibProven (dlopen) -> libproven.so -> Zig FFI -> Idris 2
 */

#ifndef PVNSafeJson_h
#define PVNSafeJson_h

#import <Foundation/Foundation.h>
#import "PVNLibProven.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * @brief Safe JSON validation and type detection.
 *
 * All validation is delegated to libproven.
 */
@interface PVNSafeJson : NSObject

/**
 * @brief Initialize with a loaded library instance.
 * @param library A loaded PVNLibProven instance.
 * @return Initialized PVNSafeJson wrapper.
 */
- (instancetype)initWithLibrary:(PVNLibProven *)library NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_UNAVAILABLE;

/**
 * @brief Check if a string is valid JSON.
 * @param json JSON string to validate.
 * @param error Output error on failure.
 * @return NSNumber wrapping BOOL (YES if valid), or nil on error.
 */
- (nullable NSNumber *)isValid:(NSString *)json
                         error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Get the JSON value type at root level.
 * @param json JSON string to inspect.
 * @return The JSON type. Returns PVNJsonTypeInvalid if not valid JSON.
 */
- (PVNJsonType)getType:(NSString *)json;

/**
 * @brief Human-readable name for a JSON type.
 * @param type The JSON type to describe.
 * @return A descriptive string (e.g. "object", "array", "string").
 */
+ (NSString *)typeDescription:(PVNJsonType)type;

@end

NS_ASSUME_NONNULL_END

#endif /* PVNSafeJson_h */
