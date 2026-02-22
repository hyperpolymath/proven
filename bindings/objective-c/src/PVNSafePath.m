// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafePath.m
 * @brief Safe filesystem path operations delegated to libproven FFI.
 *
 * All traversal detection and filename sanitization is performed by
 * the formally verified Idris 2 core. No path logic is reimplemented.
 */

#import "PVNSafePath.h"

@interface PVNSafePath ()
@property (nonatomic, strong) PVNLibProven *library;
@end

@implementation PVNSafePath

- (instancetype)initWithLibrary:(PVNLibProven *)library {
    self = [super init];
    if (self != nil) {
        _library = library;
    }
    return self;
}

- (nullable NSNumber *)hasTraversal:(NSString *)path
                              error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        NSData *utf8 = [path dataUsingEncoding:NSUTF8StringEncoding];
        if (utf8 == nil) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(PVNProvenStatusErrEncodingError);
            }
            return nil;
        }
        PVNBoolResult result = [self.library provenPathHasTraversal:(const uint8_t *)[utf8 bytes]
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

- (nullable NSString *)sanitizeFilename:(NSString *)filename
                                  error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        NSData *utf8 = [filename dataUsingEncoding:NSUTF8StringEncoding];
        if (utf8 == nil) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(PVNProvenStatusErrEncodingError);
            }
            return nil;
        }
        PVNStringResult result = [self.library provenPathSanitizeFilename:(const uint8_t *)[utf8 bytes]
                                                                   length:[utf8 length]];
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

        NSString *sanitized = [[NSString alloc] initWithBytes:result.value
                                                       length:result.length
                                                     encoding:NSUTF8StringEncoding];
        [self.library provenFreeString:result.value];

        if (sanitized == nil) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(PVNProvenStatusErrEncodingError);
            }
            return nil;
        }
        return sanitized;
    }
}

@end
