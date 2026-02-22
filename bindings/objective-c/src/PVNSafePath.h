// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafePath.h
 * @brief Safe filesystem path operations delegated to libproven FFI.
 *
 * Prevents directory traversal attacks and sanitizes filenames via
 * the formally verified Idris 2 core. No path logic is reimplemented.
 *
 * Architecture:
 *   PVNSafePath -> PVNLibProven (dlopen) -> libproven.so -> Zig FFI -> Idris 2
 */

#ifndef PVNSafePath_h
#define PVNSafePath_h

#import <Foundation/Foundation.h>
#import "PVNLibProven.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * @brief Safe filesystem path operations.
 *
 * Detects traversal sequences and sanitizes filenames to prevent
 * path-based security vulnerabilities.
 */
@interface PVNSafePath : NSObject

/**
 * @brief Initialize with a loaded library instance.
 * @param library A loaded PVNLibProven instance.
 * @return Initialized PVNSafePath wrapper.
 */
- (instancetype)initWithLibrary:(PVNLibProven *)library NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_UNAVAILABLE;

/**
 * @brief Check if a path contains directory traversal sequences ("..").
 * @param path Path string to check.
 * @param error Output error on failure.
 * @return NSNumber wrapping BOOL (YES if traversal detected), or nil on error.
 */
- (nullable NSNumber *)hasTraversal:(NSString *)path
                              error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Sanitize a filename by removing dangerous characters.
 * @param filename Filename to sanitize.
 * @param error Output error on failure.
 * @return Sanitized filename, or nil on error.
 */
- (nullable NSString *)sanitizeFilename:(NSString *)filename
                                  error:(NSError *_Nullable *_Nullable)error;

@end

NS_ASSUME_NONNULL_END

#endif /* PVNSafePath_h */
