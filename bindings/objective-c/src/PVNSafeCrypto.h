// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeCrypto.h
 * @brief Safe cryptographic operations delegated to libproven FFI.
 *
 * Provides timing-safe comparison and cryptographically secure random
 * byte generation via the formally verified Idris 2 core.
 * No cryptographic logic is reimplemented.
 *
 * Architecture:
 *   PVNSafeCrypto -> PVNLibProven (dlopen) -> libproven.so -> Zig FFI -> Idris 2
 */

#ifndef PVNSafeCrypto_h
#define PVNSafeCrypto_h

#import <Foundation/Foundation.h>
#import "PVNLibProven.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * @brief Safe cryptographic operations.
 *
 * All cryptographic operations are delegated to libproven.
 */
@interface PVNSafeCrypto : NSObject

/**
 * @brief Initialize with a loaded library instance.
 * @param library A loaded PVNLibProven instance.
 * @return Initialized PVNSafeCrypto wrapper.
 */
- (instancetype)initWithLibrary:(PVNLibProven *)library NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_UNAVAILABLE;

/**
 * @brief Constant-time byte comparison to prevent timing attacks.
 * @param dataA First byte buffer.
 * @param dataB Second byte buffer.
 * @param error Output error on failure.
 * @return NSNumber wrapping BOOL (YES if equal), or nil on error.
 *         Returns NO if lengths differ.
 */
- (nullable NSNumber *)constantTimeCompare:(NSData *)dataA
                                     withData:(NSData *)dataB
                                        error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Constant-time string comparison to prevent timing attacks.
 * @param stringA First string.
 * @param stringB Second string.
 * @param error Output error on failure.
 * @return NSNumber wrapping BOOL (YES if equal), or nil on error.
 */
- (nullable NSNumber *)constantTimeCompareString:(NSString *)stringA
                                      withString:(NSString *)stringB
                                           error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Generate cryptographically secure random bytes.
 * @param count Number of bytes to generate.
 * @param error Output error on failure.
 * @return NSData containing random bytes, or nil on error.
 */
- (nullable NSData *)randomBytes:(NSUInteger)count
                           error:(NSError *_Nullable *_Nullable)error;

@end

NS_ASSUME_NONNULL_END

#endif /* PVNSafeCrypto_h */
