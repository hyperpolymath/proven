// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * @file PVNSafeUrl.m
 * @brief Safe URL parsing delegated to libproven FFI.
 *
 * All URL parsing is performed by the formally verified Idris 2 core.
 * No parsing logic is reimplemented here. C-allocated URL component
 * memory is freed via proven_url_free after copying to NSString.
 */

#import "PVNSafeUrl.h"

// ============================================================================
// PVNParsedUrl
// ============================================================================

@implementation PVNParsedUrl

- (instancetype)initWithScheme:(NSString *)scheme
                          host:(NSString *)host
                          port:(nullable NSNumber *)port
                          path:(NSString *)path
                         query:(nullable NSString *)query
                      fragment:(nullable NSString *)fragment {
    self = [super init];
    if (self != nil) {
        _scheme   = [scheme copy];
        _host     = [host copy];
        _port     = port;
        _path     = [path copy];
        _query    = [query copy];
        _fragment = [fragment copy];
    }
    return self;
}

- (NSString *)description {
    return [NSString stringWithFormat:@"<PVNParsedUrl scheme=%@ host=%@ port=%@ path=%@ query=%@ fragment=%@>",
            self.scheme, self.host, self.port, self.path, self.query, self.fragment];
}

@end

// ============================================================================
// PVNSafeUrl
// ============================================================================

@interface PVNSafeUrl ()
@property (nonatomic, strong) PVNLibProven *library;
@end

/**
 * @brief Internal helper: safely extract an NSString from a C char pointer
 *        with a known length. Returns nil if pointer is NULL or length is 0.
 */
static NSString *_Nullable PVNStringFromCPtr(const char *_Nullable ptr, size_t len) {
    if (ptr == NULL || len == 0) {
        return nil;
    }
    return [[NSString alloc] initWithBytes:ptr length:len encoding:NSUTF8StringEncoding];
}

@implementation PVNSafeUrl

- (instancetype)initWithLibrary:(PVNLibProven *)library {
    self = [super init];
    if (self != nil) {
        _library = library;
    }
    return self;
}

- (nullable PVNParsedUrl *)parse:(NSString *)urlString
                           error:(NSError *_Nullable *_Nullable)error {
    @autoreleasepool {
        NSData *utf8 = [urlString dataUsingEncoding:NSUTF8StringEncoding];
        if (utf8 == nil) {
            if (error != NULL) {
                *error = PVNErrorFromStatus(PVNProvenStatusErrEncodingError);
            }
            return nil;
        }

        PVNUrlResult result = [self.library provenUrlParse:(const uint8_t *)[utf8 bytes]
                                                     length:[utf8 length]];
        if (result.status != PVNProvenStatusOK) {
            if (error != NULL) {
                *error = PVNErrorFromStatus((PVNProvenStatus)result.status);
            }
            return nil;
        }

        // Extract components before freeing.
        NSString *scheme = PVNStringFromCPtr(result.components.scheme,
                                             result.components.scheme_len);
        if (scheme == nil) {
            scheme = @"";
        }

        NSString *host = PVNStringFromCPtr(result.components.host,
                                           result.components.host_len);
        if (host == nil) {
            host = @"";
        }

        NSNumber *port = nil;
        if (result.components.has_port) {
            port = @(result.components.port);
        }

        NSString *path = PVNStringFromCPtr(result.components.path,
                                           result.components.path_len);
        if (path == nil) {
            path = @"/";
        }

        NSString *query = PVNStringFromCPtr(result.components.query,
                                            result.components.query_len);

        NSString *fragment = PVNStringFromCPtr(result.components.fragment,
                                               result.components.fragment_len);

        // Free C-allocated URL components.
        [self.library provenUrlFree:&result.components];

        return [[PVNParsedUrl alloc] initWithScheme:scheme
                                               host:host
                                               port:port
                                               path:path
                                              query:query
                                           fragment:fragment];
    }
}

@end
