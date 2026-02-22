// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeString.m
 * @brief Safe string operations delegated to libproven FFI.
 *
 * All escaping and validation is performed by the formally verified
 * Idris 2 core. No string escaping logic is reimplemented here.
 */

#import "PVNSafeString.h"

@interface PVNSafeString ()
@property (nonatomic, strong) PVNLibProven *library;
@end

/**
 * @brief Internal helper: convert an NSString to UTF-8 bytes, call a
 *        libproven string-returning function, and return the result
 *        as an NSString (freeing the C-allocated memory).
 */
static NSString *_Nullable PVNConsumeStringResult(PVNLibProven *library,
                                                   PVNStringResult result,
                                                   NSError *_Nullable *_Nullable error) {
    if (result.status != PVNProvenStatusOK) {
        if (error != NULL) {
            *error = PVNErrorFromStatus((PVNProvenStatus)result.status);
        }
        return nil;
    }
    if (result.value == NULL) {
        if (error != NULL) {
            *error = PVNErrorFromStatus(PVNProvenStatusErrNullPointer);
        }
        return nil;
    }

    NSString *string = [[NSString alloc] initWithBytes:result.value
                                                length:result.length
                                              encoding:NSUTF8StringEncoding];
    [library provenFreeString:result.value];

    if (string == nil) {
        if (error != NULL) {
            *error = PVNErrorFromStatus(PVNProvenStatusErrEncodingError);
        }
        return nil;
    }
    return string;
}

@implementation PVNSafeString

- (instancetype)initWithLibrary:(PVNLibProven *)library {
    self = [super init];
    if (self != nil) {
        _library = library;
    }
    return self;
}

- (nullable NSNumber *)isValidUtf8:(NSData *)data
                             error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        const uint8_t *ptr = (const uint8_t *)[data bytes];
        size_t len = [data length];
        PVNBoolResult result = [self.library provenStringIsValidUtf8:ptr length:len];
        if (result.status != PVNProvenStatusOK) {
            if (error != NULL) {
                *error = PVNErrorFromStatus((PVNProvenStatus)result.status);
            }
            return nil;
        }
        return @(result.value);
    }
}

- (nullable NSString *)escapeSql:(NSString *)value
                           error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        NSData *utf8 = [value dataUsingEncoding:NSUTF8StringEncoding];
        if (utf8 == nil) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(PVNProvenStatusErrEncodingError);
            }
            return nil;
        }
        PVNStringResult result = [self.library provenStringEscapeSql:(const uint8_t *)[utf8 bytes]
                                                              length:[utf8 length]];
        return PVNConsumeStringResult(self.library, result, error);
    }
}

- (nullable NSString *)escapeHtml:(NSString *)value
                            error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        NSData *utf8 = [value dataUsingEncoding:NSUTF8StringEncoding];
        if (utf8 == nil) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(PVNProvenStatusErrEncodingError);
            }
            return nil;
        }
        PVNStringResult result = [self.library provenStringEscapeHtml:(const uint8_t *)[utf8 bytes]
                                                               length:[utf8 length]];
        return PVNConsumeStringResult(self.library, result, error);
    }
}

- (nullable NSString *)escapeJs:(NSString *)value
                          error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        NSData *utf8 = [value dataUsingEncoding:NSUTF8StringEncoding];
        if (utf8 == nil) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(PVNProvenStatusErrEncodingError);
            }
            return nil;
        }
        PVNStringResult result = [self.library provenStringEscapeJs:(const uint8_t *)[utf8 bytes]
                                                              length:[utf8 length]];
        return PVNConsumeStringResult(self.library, result, error);
    }
}

@end
