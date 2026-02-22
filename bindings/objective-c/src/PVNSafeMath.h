// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeMath.h
 * @brief Safe arithmetic operations delegated to libproven FFI.
 *
 * All computation is performed by the formally verified Idris 2 core
 * via the Zig FFI bridge. This class provides Objective-C idiomatic
 * wrappers returning nullable NSNumber results.
 *
 * Architecture:
 *   PVNSafeMath -> PVNLibProven (dlopen) -> libproven.so -> Zig FFI -> Idris 2
 */

#ifndef PVNSafeMath_h
#define PVNSafeMath_h

#import <Foundation/Foundation.h>
#import "PVNLibProven.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * @brief Safe arithmetic operations that never crash.
 *
 * All methods delegate to libproven. Returns nil (with NSError) on
 * overflow, underflow, division by zero, or other error conditions.
 */
@interface PVNSafeMath : NSObject

/**
 * @brief Initialize with a loaded library instance.
 * @param library A loaded PVNLibProven instance.
 * @return Initialized PVNSafeMath wrapper.
 */
- (instancetype)initWithLibrary:(PVNLibProven *)library NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_UNAVAILABLE;

/**
 * @brief Checked addition with overflow detection.
 * @param a First operand.
 * @param b Second operand.
 * @param error Output error if overflow occurs.
 * @return Sum as NSNumber, or nil on overflow.
 */
- (nullable NSNumber *)addChecked:(int64_t)a
                                b:(int64_t)b
                            error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Checked subtraction with underflow detection.
 * @param a First operand.
 * @param b Second operand.
 * @param error Output error if underflow occurs.
 * @return Difference as NSNumber, or nil on underflow.
 */
- (nullable NSNumber *)subChecked:(int64_t)a
                                b:(int64_t)b
                            error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Checked multiplication with overflow detection.
 * @param a First operand.
 * @param b Second operand.
 * @param error Output error if overflow occurs.
 * @return Product as NSNumber, or nil on overflow.
 */
- (nullable NSNumber *)mulChecked:(int64_t)a
                                b:(int64_t)b
                            error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Safe integer division.
 * @param numerator Dividend.
 * @param denominator Divisor.
 * @param error Output error if division by zero or overflow.
 * @return Quotient as NSNumber, or nil on error.
 */
- (nullable NSNumber *)div:(int64_t)numerator
               denominator:(int64_t)denominator
                     error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Safe modulo operation.
 * @param numerator Dividend.
 * @param denominator Divisor.
 * @param error Output error if division by zero.
 * @return Remainder as NSNumber, or nil on error.
 */
- (nullable NSNumber *)mod:(int64_t)numerator
               denominator:(int64_t)denominator
                     error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Safe absolute value.
 * @param n Input value.
 * @param error Output error if overflow (INT64_MIN cannot be represented).
 * @return Absolute value as NSNumber, or nil on overflow.
 */
- (nullable NSNumber *)absSafe:(int64_t)n
                         error:(NSError *_Nullable *_Nullable)error;

/**
 * @brief Clamp value to [lo, hi] range.
 * @param lo Lower bound (inclusive).
 * @param hi Upper bound (inclusive).
 * @param value Value to clamp.
 * @return Clamped value.
 */
- (int64_t)clampLo:(int64_t)lo hi:(int64_t)hi value:(int64_t)value;

/**
 * @brief Integer exponentiation with overflow checking.
 * @param base Base value.
 * @param exp Exponent (non-negative).
 * @param error Output error if overflow.
 * @return Result as NSNumber, or nil on overflow.
 */
- (nullable NSNumber *)powChecked:(int64_t)base
                              exp:(uint32_t)exp
                            error:(NSError *_Nullable *_Nullable)error;

@end

NS_ASSUME_NONNULL_END

#endif /* PVNSafeMath_h */
