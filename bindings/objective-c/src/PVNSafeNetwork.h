// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeNetwork.h
 * @brief Safe network operations delegated to libproven FFI.
 *
 * IPv4 parsing and classification performed by the formally verified
 * Idris 2 core via the Zig FFI bridge. No network logic is reimplemented.
 *
 * Architecture:
 *   PVNSafeNetwork -> PVNLibProven (dlopen) -> libproven.so -> Zig FFI -> Idris 2
 */

#ifndef PVNSafeNetwork_h
#define PVNSafeNetwork_h

#import <Foundation/Foundation.h>
#import "PVNLibProven.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * @brief Represents a parsed IPv4 address.
 */
@interface PVNIPv4 : NSObject

/** @brief First octet. */
@property (nonatomic, readonly) uint8_t octet0;
/** @brief Second octet. */
@property (nonatomic, readonly) uint8_t octet1;
/** @brief Third octet. */
@property (nonatomic, readonly) uint8_t octet2;
/** @brief Fourth octet. */
@property (nonatomic, readonly) uint8_t octet3;

/**
 * @brief Initialize with four octets.
 */
- (instancetype)initWithOctet0:(uint8_t)o0
                        octet1:(uint8_t)o1
                        octet2:(uint8_t)o2
                        octet3:(uint8_t)o3 NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_UNAVAILABLE;

/**
 * @brief Returns the C-level PVNIPv4Address struct for FFI calls.
 */
- (PVNIPv4Address)toCAddress;

@end

/**
 * @brief Safe network operations.
 *
 * All parsing and classification is delegated to libproven.
 */
@interface PVNSafeNetwork : NSObject

/**
 * @brief Initialize with a loaded library instance.
 * @param library A loaded PVNLibProven instance.
 * @return Initialized PVNSafeNetwork wrapper.
 */
- (instancetype)initWithLibrary:(PVNLibProven *)library NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_UNAVAILABLE;

/**
 * @brief Parse an IPv4 address string (e.g. "192.168.1.1").
 * @param address IPv4 address string.
 * @param error Output error if parsing fails.
 * @return Parsed IPv4 address, or nil on error.
 */
- (nullable PVNIPv4 *)parseIPv4:(NSString *)address
                          error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Check if an IPv4 address is private (RFC 1918).
 * @param address Parsed IPv4 address.
 * @return YES if private (10.x.x.x, 172.16-31.x.x, 192.168.x.x).
 */
- (BOOL)isPrivate:(PVNIPv4 *)address;

/**
 * @brief Check if an IPv4 address is loopback (127.0.0.0/8).
 * @param address Parsed IPv4 address.
 * @return YES if loopback.
 */
- (BOOL)isLoopback:(PVNIPv4 *)address;

/**
 * @brief Parse and check if an IPv4 address string is private.
 * @param addressString IPv4 address string.
 * @param error Output error if parsing fails.
 * @return NSNumber wrapping BOOL, or nil on parse error.
 */
- (nullable NSNumber *)isPrivateString:(NSString *)addressString
                                 error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Parse and check if an IPv4 address string is loopback.
 * @param addressString IPv4 address string.
 * @param error Output error if parsing fails.
 * @return NSNumber wrapping BOOL, or nil on parse error.
 */
- (nullable NSNumber *)isLoopbackString:(NSString *)addressString
                                  error:(NSError *_Nullable *_Nullable)error;

@end

NS_ASSUME_NONNULL_END

#endif /* PVNSafeNetwork_h */
