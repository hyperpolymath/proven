// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeJson.m
 * @brief Safe JSON validation delegated to libproven FFI.
 *
 * All JSON validation and type detection is performed by the formally
 * verified Idris 2 core. No JSON logic is reimplemented here.
 */

#import "PVNSafeJson.h"

@interface PVNSafeJson ()
@property (nonatomic, strong) PVNLibProven *library;
@end

@implementation PVNSafeJson

- (instancetype)initWithLibrary:(PVNLibProven *)library {
    self = [super init];
    if (self != nil) {
        _library = library;
    }
    return self;
}

- (nullable NSNumber *)isValid:(NSString *)json
                         error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        NSData *utf8 = [json dataUsingEncoding:NSUTF8StringEncoding];
        if (utf8 == nil) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(PVNProvenStatusErrEncodingError);
            }
            return nil;
        }
        PVNBoolResult result = [self.library provenJsonIsValid:(const uint8_t *)[utf8 bytes]
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

- (PVNJsonType)getType:(NSString *)json {
    @autoreleasepool {
        NSData *utf8 = [json dataUsingEncoding:NSUTF8StringEncoding];
        if (utf8 == nil) {
            return PVNJsonTypeInvalid;
        }
        return [self.library provenJsonGetType:(const uint8_t *)[utf8 bytes]
                                        length:[utf8 length]];
    }
}

+ (NSString *)typeDescription:(PVNJsonType)type {
    switch (type) {
        case PVNJsonTypeNull:    return @"null";
        case PVNJsonTypeBool:    return @"bool";
        case PVNJsonTypeNumber:  return @"number";
        case PVNJsonTypeString:  return @"string";
        case PVNJsonTypeArray:   return @"array";
        case PVNJsonTypeObject:  return @"object";
        case PVNJsonTypeInvalid: return @"invalid";
        default:                 return @"unknown";
    }
}

@end
