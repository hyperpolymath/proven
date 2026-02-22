// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeCrypto.m
 * @brief Safe cryptographic operations delegated to libproven FFI.
 *
 * All cryptographic operations are performed by the formally verified
 * Idris 2 core. No cryptographic logic is reimplemented here.
 */

#import "PVNSafeCrypto.h"

@interface PVNSafeCrypto ()
@property (nonatomic, strong) PVNLibProven *library;
@end

@implementation PVNSafeCrypto

- (instancetype)initWithLibrary:(PVNLibProven *)library {
    self = [super init];
    if (self != nil) {
        _library = library;
    }
    return self;
}

- (nullable NSNumber *)constantTimeCompare:(NSData *)dataA
                                     withData:(NSData *)dataB
                                        error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        const uint8_t *ptrA = (const uint8_t *)[dataA bytes];
        size_t lenA = [dataA length];
        const uint8_t *ptrB = (const uint8_t *)[dataB bytes];
        size_t lenB = [dataB length];

        PVNBoolResult result = [self.library provenCryptoConstantTimeEq:ptrA
                                                                length1:lenA
                                                                   ptr2:ptrB
                                                                length2:lenB];
        if (result.status != PVNProvenStatusOK) {
            if (error != NULL) {
                *error = PVNErrorFromStatus((PVNProvenStatus)result.status);
            }
            return nil;
        }
        return @(result.value);
    }
}

- (nullable NSNumber *)constantTimeCompareString:(NSString *)stringA
                                      withString:(NSString *)stringB
                                           error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        NSData *utf8A = [stringA dataUsingEncoding:NSUTF8StringEncoding];
        NSData *utf8B = [stringB dataUsingEncoding:NSUTF8StringEncoding];
        if (utf8A == nil || utf8B == nil) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(PVNProvenStatusErrEncodingError);
            }
            return nil;
        }
        return [self constantTimeCompare:utf8A withData:utf8B error:error];
    }
}

- (nullable NSData *)randomBytes:(NSUInteger)count
                           error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        if (count == 0) {
            return [NSData data];
        }

        NSMutableData *buffer = [NSMutableData dataWithLength:count];
        if (buffer == nil) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(PVNProvenStatusErrAllocationFailed);
            }
            return nil;
        }

        PVNProvenStatus status = [self.library provenCryptoRandomBytes:(uint8_t *)[buffer mutableBytes]
                                                                length:count];
        if (status != PVNProvenStatusOK) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(status);
            }
            return nil;
        }
        return [buffer copy];
    }
}

@end
