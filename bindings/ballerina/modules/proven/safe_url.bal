// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_url.bal - Safe URL parsing and validation via libproven FFI.
//
// Parses URLs into components following RFC 3986.
// All operations delegate to Idris 2 verified code.
// No URL parsing logic is implemented in Ballerina.

// Parsed URL components.
public type ParsedUrl record {|
    string scheme;
    string host;
    int? port;
    string path;
    string query;
    string fragment;
|};

// Parse a URL string into its components.
//
// Returns a ParsedUrl record with each component extracted.
// Components not present in the input URL will be empty strings.
// Returns error on parse failure.
public isolated function parseUrl(string input) returns ParsedUrl|error {
    byte[] data = input.toBytes();
    handle nativePtr = toNativeBytes(data);
    handle result = provenUrlParse(nativePtr, data.length());
    freeNativeBytes(nativePtr);

    // The JNA bridge extracts the status from the ProvenUrlResult struct.
    // On success, it copies all string components to Java strings and frees
    // the native URL components. On failure, it returns the error status.
    int status = extractUrlResultStatus(result);
    if succeeded(status) {
        ParsedUrl parsed = {
            scheme: extractUrlScheme(result),
            host: extractUrlHost(result),
            port: extractUrlPort(result),
            path: extractUrlPath(result),
            query: extractUrlQuery(result),
            fragment: extractUrlFragment(result)
        };
        freeUrlResult(result);
        return parsed;
    }
    return statusToError(status);
}

// JNA helper: Extract status from URL result.
isolated function extractUrlResultStatus(handle result) returns int = @java:Method {
    name: "extractUrlResultStatus",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract scheme from URL result.
isolated function extractUrlScheme(handle result) returns string = @java:Method {
    name: "extractUrlScheme",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract host from URL result.
isolated function extractUrlHost(handle result) returns string = @java:Method {
    name: "extractUrlHost",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract port from URL result (returns -1 if no port).
isolated function extractUrlPort(handle result) returns int? = @java:Method {
    name: "extractUrlPort",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract path from URL result.
isolated function extractUrlPath(handle result) returns string = @java:Method {
    name: "extractUrlPath",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract query from URL result.
isolated function extractUrlQuery(handle result) returns string = @java:Method {
    name: "extractUrlQuery",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Extract fragment from URL result.
isolated function extractUrlFragment(handle result) returns string = @java:Method {
    name: "extractUrlFragment",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;

// JNA helper: Free URL result and its native components.
isolated function freeUrlResult(handle result) = @java:Method {
    name: "freeUrlResult",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;
