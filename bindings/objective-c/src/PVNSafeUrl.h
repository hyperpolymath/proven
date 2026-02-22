// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeUrl.h
 * @brief Safe URL parsing and validation delegated to libproven FFI.
 *
 * All URL parsing is performed by the formally verified Idris 2 core
 * via the Zig FFI bridge. No parsing logic is reimplemented.
 *
 * Architecture:
 *   PVNSafeUrl -> PVNLibProven (dlopen) -> libproven.so -> Zig FFI -> Idris 2
 */

#ifndef PVNSafeUrl_h
#define PVNSafeUrl_h

#import <Foundation/Foundation.h>
#import "PVNLibProven.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * @brief Parsed URL components returned by PVNSafeUrl.
 */
@interface PVNParsedUrl : NSObject

/** @brief URL scheme (e.g. "https"). */
@property (nonatomic, readonly, copy) NSString *scheme;

/** @brief Hostname (e.g. "example.com"). */
@property (nonatomic, readonly, copy) NSString *host;

/** @brief Port number, or nil if not specified. */
@property (nonatomic, readonly, nullable) NSNumber *port;

/** @brief URL path (e.g. "/api/v1"). */
@property (nonatomic, readonly, copy) NSString *path;

/** @brief Query string, or nil if absent. */
@property (nonatomic, readonly, copy, nullable) NSString *query;

/** @brief Fragment identifier, or nil if absent. */
@property (nonatomic, readonly, copy, nullable) NSString *fragment;

/**
 * @brief Initialize with all URL components.
 */
- (instancetype)initWithScheme:(NSString *)scheme
                          host:(NSString *)host
                          port:(nullable NSNumber *)port
                          path:(NSString *)path
                         query:(nullable NSString *)query
                      fragment:(nullable NSString *)fragment NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_UNAVAILABLE;

@end

/**
 * @brief Safe URL parsing operations.
 *
 * All parsing is delegated to libproven.
 */
@interface PVNSafeUrl : NSObject

/**
 * @brief Initialize with a loaded library instance.
 * @param library A loaded PVNLibProven instance.
 * @return Initialized PVNSafeUrl wrapper.
 */
- (instancetype)initWithLibrary:(PVNLibProven *)library NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_UNAVAILABLE;

/**
 * @brief Parse a URL string into its components.
 * @param urlString URL string to parse.
 * @param error Output error if parsing fails.
 * @return Parsed URL components, or nil on error.
 */
- (nullable PVNParsedUrl *)parse:(NSString *)urlString
                           error:(NSError *_Nullable *_Nullable)error;

@end

NS_ASSUME_NONNULL_END

#endif /* PVNSafeUrl_h */
