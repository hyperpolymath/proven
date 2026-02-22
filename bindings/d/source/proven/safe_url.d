// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe URL parsing and validation operations.
 *
 * Thin FFI wrapper around libproven's SafeUrl module. All URL parsing
 * is performed in formally verified Idris 2 code. This module only
 * marshals data to/from the C ABI.
 */
module proven.safe_url;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Parsed URL components.
struct ParsedUrl
{
    /// URL scheme (e.g., "https")
    string scheme;
    /// Host (domain or IP)
    string host;
    /// Optional port
    Nullable!ushort port;
    /// Path component
    string path;
    /// Optional query string (without ?)
    Nullable!string query;
    /// Optional fragment (without #)
    Nullable!string fragment;
}

/// URL parsing result type.
struct UrlResult
{
    ParsedUrl url;
    string error;
    bool ok;

    static UrlResult success(ParsedUrl url)
    {
        return UrlResult(url, "", true);
    }

    static UrlResult failure(string error)
    {
        return UrlResult(ParsedUrl.init, error, false);
    }
}

/// Parse a URL string into its components.
UrlResult parseUrl(string urlString) @trusted nothrow
{
    if (urlString.length == 0)
        return UrlResult.failure("Empty URL");

    auto result = proven_url_parse(
        cast(const(ubyte)*) urlString.ptr, urlString.length
    );

    if (provenFailed(result.status))
        return UrlResult.failure("URL parse failed");

    // Extract D strings from C result, copying data before freeing
    auto components = result.components;

    string scheme = "";
    if (components.scheme !is null && components.scheme_len > 0)
        scheme = components.scheme[0 .. components.scheme_len].idup;

    string host = "";
    if (components.host !is null && components.host_len > 0)
        host = components.host[0 .. components.host_len].idup;

    Nullable!ushort port;
    if (components.has_port)
        port = nullable(components.port);

    string path = "/";
    if (components.path !is null && components.path_len > 0)
        path = components.path[0 .. components.path_len].idup;

    Nullable!string query;
    if (components.query !is null && components.query_len > 0)
        query = nullable(components.query[0 .. components.query_len].idup);

    Nullable!string fragment;
    if (components.fragment !is null && components.fragment_len > 0)
        fragment = nullable(components.fragment[0 .. components.fragment_len].idup);

    // Free the C-allocated URL components
    proven_url_free(&result.components);

    return UrlResult.success(ParsedUrl(scheme, host, port, path, query, fragment));
}

/// Check if URL is valid.
bool isValidUrl(string url) @trusted nothrow
{
    return parseUrl(url).ok;
}

/// Get domain from URL.
Nullable!string getDomain(string url) @trusted nothrow
{
    auto result = parseUrl(url);
    if (!result.ok)
        return Nullable!string.init;
    return nullable(result.url.host);
}

/// Check if URL uses HTTPS.
bool isHttps(string url) @trusted nothrow
{
    auto result = parseUrl(url);
    if (!result.ok)
        return false;
    return result.url.scheme == "https";
}

/// Check if URL is secure (HTTPS, WSS, etc).
bool isSecureScheme(string url) @trusted nothrow
{
    auto result = parseUrl(url);
    if (!result.ok)
        return false;
    return result.url.scheme == "https" ||
           result.url.scheme == "wss" ||
           result.url.scheme == "ftps";
}
