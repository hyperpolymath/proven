// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeEmail.h
 * @brief Safe email validation delegated to libproven FFI.
 *
 * RFC 5321 compliant validation performed by the formally verified
 * Idris 2 core via the Zig FFI bridge. No validation logic is reimplemented.
 *
 * Architecture:
 *   PVNSafeEmail -> PVNLibProven (dlopen) -> libproven.so -> Zig FFI -> Idris 2
 */

#ifndef PVNSafeEmail_h
#define PVNSafeEmail_h

#import <Foundation/Foundation.h>
#import "PVNLibProven.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * @brief Safe email validation.
 *
 * All validation is delegated to libproven.
 */
@interface PVNSafeEmail : NSObject

/**
 * @brief Initialize with a loaded library instance.
 * @param library A loaded PVNLibProven instance.
 * @return Initialized PVNSafeEmail wrapper.
 */
- (instancetype)initWithLibrary:(PVNLibProven *)library NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_UNAVAILABLE;

/**
 * @brief Validate an email address (RFC 5321 simplified).
 * @param email Email address string to validate.
 * @param error Output error on failure.
 * @return NSNumber wrapping BOOL (YES if valid), or nil on error.
 */
- (nullable NSNumber *)isValid:(NSString *)email
                         error:(NSError *_Nullable *_Nullable)error;

@end

NS_ASSUME_NONNULL_END

#endif /* PVNSafeEmail_h */
