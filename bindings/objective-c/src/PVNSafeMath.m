// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeMath.m
 * @brief Safe arithmetic operations delegated to libproven FFI.
 *
 * All computation is delegated to the formally verified Idris 2 core.
 * No arithmetic logic is reimplemented here.
 */

#import "PVNSafeMath.h"

@interface PVNSafeMath ()
@property (nonatomic, strong) PVNLibProven *library;
@end

@implementation PVNSafeMath

- (instancetype)initWithLibrary:(PVNLibProven *)library {
    self = [super init];
    if (self != nil) {
        _library = library;
    }
    return self;
}

- (nullable NSNumber *)addChecked:(int64_t)a
                                b:(int64_t)b
                            error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        PVNIntResult result = [self.library provenMathAddChecked:a b:b];
        if (result.status != PVNProvenStatusOK) {
            if (error != NULL) {
                *error = PVNErrorFromStatus((PVNProvenStatus)result.status);
            }
            return nil;
        }
        return @(result.value);
    }
}

- (nullable NSNumber *)subChecked:(int64_t)a
                                b:(int64_t)b
                            error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        PVNIntResult result = [self.library provenMathSubChecked:a b:b];
        if (result.status != PVNProvenStatusOK) {
            if (error != NULL) {
                *error = PVNErrorFromStatus((PVNProvenStatus)result.status);
            }
            return nil;
        }
        return @(result.value);
    }
}

- (nullable NSNumber *)mulChecked:(int64_t)a
                                b:(int64_t)b
                            error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        PVNIntResult result = [self.library provenMathMulChecked:a b:b];
        if (result.status != PVNProvenStatusOK) {
            if (error != NULL) {
                *error = PVNErrorFromStatus((PVNProvenStatus)result.status);
            }
            return nil;
        }
        return @(result.value);
    }
}

- (nullable NSNumber *)div:(int64_t)numerator
               denominator:(int64_t)denominator
                     error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        PVNIntResult result = [self.library provenMathDiv:numerator denominator:denominator];
        if (result.status != PVNProvenStatusOK) {
            if (error != NULL) {
                *error = PVNErrorFromStatus((PVNProvenStatus)result.status);
            }
            return nil;
        }
        return @(result.value);
    }
}

- (nullable NSNumber *)mod:(int64_t)numerator
               denominator:(int64_t)denominator
                     error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        PVNIntResult result = [self.library provenMathMod:numerator denominator:denominator];
        if (result.status != PVNProvenStatusOK) {
            if (error != NULL) {
                *error = PVNErrorFromStatus((PVNProvenStatus)result.status);
            }
            return nil;
        }
        return @(result.value);
    }
}

- (nullable NSNumber *)absSafe:(int64_t)n
                         error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        PVNIntResult result = [self.library provenMathAbsSafe:n];
        if (result.status != PVNProvenStatusOK) {
            if (error != NULL) {
                *error = PVNErrorFromStatus((PVNProvenStatus)result.status);
            }
            return nil;
        }
        return @(result.value);
    }
}

- (int64_t)clampLo:(int64_t)lo hi:(int64_t)hi value:(int64_t)value {
    return [self.library provenMathClamp:lo hi:hi value:value];
}

- (nullable NSNumber *)powChecked:(int64_t)base
                              exp:(uint32_t)exp
                            error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        PVNIntResult result = [self.library provenMathPowChecked:base exp:exp];
        if (result.status != PVNProvenStatusOK) {
            if (error != NULL) {
                *error = PVNErrorFromStatus((PVNProvenStatus)result.status);
            }
            return nil;
        }
        return @(result.value);
    }
}

@end
