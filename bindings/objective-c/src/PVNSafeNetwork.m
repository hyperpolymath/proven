// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeNetwork.m
 * @brief Safe network operations delegated to libproven FFI.
 *
 * IPv4 parsing and classification is performed by the formally verified
 * Idris 2 core. No network logic is reimplemented here.
 */

#import "PVNSafeNetwork.h"

// ============================================================================
// PVNIPv4
// ============================================================================

@implementation PVNIPv4

- (instancetype)initWithOctet0:(uint8_t)o0
                        octet1:(uint8_t)o1
                        octet2:(uint8_t)o2
                        octet3:(uint8_t)o3 {
    self = [super init];
    if (self != nil) {
        _octet0 = o0;
        _octet1 = o1;
        _octet2 = o2;
        _octet3 = o3;
    }
    return self;
}

- (PVNIPv4Address)toCAddress {
    PVNIPv4Address addr;
    addr.octets[0] = self.octet0;
    addr.octets[1] = self.octet1;
    addr.octets[2] = self.octet2;
    addr.octets[3] = self.octet3;
    return addr;
}

- (NSString *)description {
    return [NSString stringWithFormat:@"%u.%u.%u.%u",
            self.octet0, self.octet1, self.octet2, self.octet3];
}

- (BOOL)isEqual:(id)object {
    if (self == object) {
        return YES;
    }
    if (![object isKindOfClass:[PVNIPv4 class]]) {
        return NO;
    }
    PVNIPv4 *other = (PVNIPv4 *)object;
    return self.octet0 == other.octet0
        && self.octet1 == other.octet1
        && self.octet2 == other.octet2
        && self.octet3 == other.octet3;
}

- (NSUInteger)hash {
    return ((NSUInteger)self.octet0 << 24)
         | ((NSUInteger)self.octet1 << 16)
         | ((NSUInteger)self.octet2 << 8)
         | (NSUInteger)self.octet3;
}

@end

// ============================================================================
// PVNSafeNetwork
// ============================================================================

@interface PVNSafeNetwork ()
@property (nonatomic, strong) PVNLibProven *library;
@end

@implementation PVNSafeNetwork

- (instancetype)initWithLibrary:(PVNLibProven *)library {
    self = [super init];
    if (self != nil) {
        _library = library;
    }
    return self;
}

- (nullable PVNIPv4 *)parseIPv4:(NSString *)address
                          error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        NSData *utf8 = [address dataUsingEncoding:NSUTF8StringEncoding];
        if (utf8 == nil) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(PVNProvenStatusErrEncodingError);
            }
            return nil;
        }
        PVNIPv4Result result = [self.library provenNetworkParseIPv4:(const uint8_t *)[utf8 bytes]
                                                             length:[utf8 length]];
        if (result.status != PVNProvenStatusOK) {
            if (error != NULL) {
                *error = PVNErrorFromStatus((PVNProvenStatus)result.status);
            }
            return nil;
        }
        return [[PVNIPv4 alloc] initWithOctet0:result.address.octets[0]
                                        octet1:result.address.octets[1]
                                        octet2:result.address.octets[2]
                                        octet3:result.address.octets[3]];
    }
}

- (BOOL)isPrivate:(PVNIPv4 *)address {
    PVNIPv4Address cAddr = [address toCAddress];
    return [self.library provenNetworkIPv4IsPrivate:cAddr];
}

- (BOOL)isLoopback:(PVNIPv4 *)address {
    PVNIPv4Address cAddr = [address toCAddress];
    return [self.library provenNetworkIPv4IsLoopback:cAddr];
}

- (nullable NSNumber *)isPrivateString:(NSString *)addressString
                                 error:(NSError *_Nullable *_Nullable)error {
    PVNIPv4 *parsed = [self parseIPv4:addressString error:error];
    if (parsed == nil) {
        return nil;
    }
    return @([self isPrivate:parsed]);
}

- (nullable NSNumber *)isLoopbackString:(NSString *)addressString
                                  error:(NSError *_Nullable *_Nullable)error {
    PVNIPv4 *parsed = [self parseIPv4:addressString error:error];
    if (parsed == nil) {
        return nil;
    }
    return @([self isLoopback:parsed]);
}

@end
