// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeEmail.m
 * @brief Safe email validation delegated to libproven FFI.
 *
 * RFC 5321 compliant validation. All logic lives in the formally
 * verified Idris 2 core. No validation is reimplemented here.
 */

#import "PVNSafeEmail.h"

@interface PVNSafeEmail ()
@property (nonatomic, strong) PVNLibProven *library;
@end

@implementation PVNSafeEmail

- (instancetype)initWithLibrary:(PVNLibProven *)library {
    self = [super init];
    if (self != nil) {
        _library = library;
    }
    return self;
}

- (nullable NSNumber *)isValid:(NSString *)email
                         error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        NSData *utf8 = [email dataUsingEncoding:NSUTF8StringEncoding];
        if (utf8 == nil) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(PVNProvenStatusErrEncodingError);
            }
            return nil;
        }
        PVNBoolResult result = [self.library provenEmailIsValid:(const uint8_t *)[utf8 bytes]
                                                         length:[utf8 length]];
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
