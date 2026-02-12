// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe URL parsing and manipulation operations.
 * Provides validated URL parsing with scheme, host, port, path,
 * query, and fragment extraction.
 */
module proven.safe_url;

import std.algorithm : canFind, startsWith;
import std.array : appender, split;
import std.conv : to, ConvException;
import std.string : toLower, indexOf, strip;
import std.typecons : Nullable, nullable;

/// Parsed URL components.
struct ParsedUrl
{
    /// URL scheme (e.g., "https")
    string scheme;
    /// Optional username
    Nullable!string username;
    /// Optional password
    Nullable!string password;
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

/// Parse a URL string.
UrlResult parseUrl(string urlString) pure @safe
{
    auto url = urlString.strip();
    if (url.length == 0)
        return UrlResult.failure("Empty URL");

    // Extract scheme
    auto schemeEnd = url.indexOf("://");
    if (schemeEnd < 0)
        return UrlResult.failure("Missing scheme (expected ://)");

    immutable scheme = url[0 .. schemeEnd].toLower();
    auto rest = url[schemeEnd + 3 .. $];

    // Extract fragment
    Nullable!string fragment;
    auto fragmentIdx = rest.indexOf('#');
    if (fragmentIdx >= 0)
    {
        fragment = nullable(rest[fragmentIdx + 1 .. $]);
        rest = rest[0 .. fragmentIdx];
    }

    // Extract query
    Nullable!string query;
    auto queryIdx = rest.indexOf('?');
    if (queryIdx >= 0)
    {
        query = nullable(rest[queryIdx + 1 .. $]);
        rest = rest[0 .. queryIdx];
    }

    // Extract path
    string path = "/";
    auto pathIdx = rest.indexOf('/');
    if (pathIdx >= 0)
    {
        path = rest[pathIdx .. $];
        rest = rest[0 .. pathIdx];
    }

    // Extract userinfo
    Nullable!string username;
    Nullable!string password;
    auto atIdx = rest.indexOf('@');
    if (atIdx >= 0)
    {
        auto userinfo = rest[0 .. atIdx];
        rest = rest[atIdx + 1 .. $];
        auto colonIdx = userinfo.indexOf(':');
        if (colonIdx >= 0)
        {
            username = nullable(userinfo[0 .. colonIdx]);
            password = nullable(userinfo[colonIdx + 1 .. $]);
        }
        else
        {
            username = nullable(userinfo);
        }
    }

    // Extract port
    Nullable!ushort port;
    string host = rest;

    // Handle IPv6 addresses
    if (host.startsWith("["))
    {
        auto bracketEnd = host.indexOf(']');
        if (bracketEnd < 0)
            return UrlResult.failure("Invalid IPv6 address (missing ])");

        auto afterBracket = host[bracketEnd + 1 .. $];
        host = host[1 .. bracketEnd];

        if (afterBracket.startsWith(":"))
        {
            try
            {
                port = nullable(afterBracket[1 .. $].to!ushort);
            }
            catch (ConvException)
            {
                return UrlResult.failure("Invalid port number");
            }
        }
    }
    else
    {
        auto colonIdx = host.indexOf(':');
        if (colonIdx >= 0)
        {
            try
            {
                port = nullable(host[colonIdx + 1 .. $].to!ushort);
            }
            catch (ConvException)
            {
                return UrlResult.failure("Invalid port number");
            }
            host = host[0 .. colonIdx];
        }
    }

    if (host.length == 0)
        return UrlResult.failure("Missing host");

    return UrlResult.success(ParsedUrl(
        scheme,
        username,
        password,
        host,
        port,
        path,
        query,
        fragment
    ));
}

/// Check if URL is valid.
bool isValidUrl(string url) pure @safe
{
    return parseUrl(url).ok;
}

/// Get domain from URL.
Nullable!string getDomain(string url) pure @safe
{
    auto result = parseUrl(url);
    if (!result.ok)
        return Nullable!string.init;
    return nullable(result.url.host);
}

/// Check if URL uses HTTPS.
bool isHttps(string url) pure @safe
{
    auto result = parseUrl(url);
    if (!result.ok)
        return false;
    return result.url.scheme == "https";
}

/// Check if URL is secure (HTTPS, WSS, etc).
bool isSecureScheme(string url) pure @safe
{
    auto result = parseUrl(url);
    if (!result.ok)
        return false;
    return result.url.scheme == "https" || result.url.scheme == "wss" || result.url.scheme == "ftps";
}

/// Get the origin (scheme + host + port).
Nullable!string getOrigin(string url) pure @safe
{
    auto result = parseUrl(url);
    if (!result.ok)
        return Nullable!string.init;

    auto origin = result.url.scheme ~ "://" ~ result.url.host;
    if (!result.url.port.isNull)
    {
        import std.format : format;
        origin ~= format!":%d"(result.url.port.get);
    }
    return nullable(origin);
}

/// Extract query parameters as key-value pairs.
string[string] parseQueryParams(string queryString) pure @safe
{
    string[string] params;
    if (queryString.length == 0)
        return params;

    // Handle leading ?
    auto query = queryString;
    if (query.startsWith("?"))
        query = query[1 .. $];

    auto pairs = query.split('&');
    foreach (pair; pairs)
    {
        auto eqIdx = pair.indexOf('=');
        if (eqIdx >= 0)
        {
            params[pair[0 .. eqIdx]] = pair[eqIdx + 1 .. $];
        }
        else
        {
            params[pair] = "";
        }
    }
    return params;
}

/// Build URL from components.
string buildUrl(string scheme, string host, ushort port = 0, string path = "/",
    string query = "", string fragment = "") pure @safe
{
    auto result = appender!string;
    result ~= scheme;
    result ~= "://";
    result ~= host;

    if (port != 0)
    {
        import std.format : format;
        result ~= format!":%d"(port);
    }

    result ~= path;

    if (query.length > 0)
    {
        result ~= '?';
        result ~= query;
    }

    if (fragment.length > 0)
    {
        result ~= '#';
        result ~= fragment;
    }

    return result[];
}

// Unit tests
unittest
{
    // Test basic URL parsing
    auto result = parseUrl("https://example.com/path");
    assert(result.ok);
    assert(result.url.scheme == "https");
    assert(result.url.host == "example.com");
    assert(result.url.path == "/path");

    // Test URL with port
    auto withPort = parseUrl("http://localhost:8080/api");
    assert(withPort.ok);
    assert(withPort.url.host == "localhost");
    assert(withPort.url.port.get == 8080);

    // Test URL with query and fragment
    auto full = parseUrl("https://example.com/search?q=test#results");
    assert(full.ok);
    assert(full.url.query.get == "q=test");
    assert(full.url.fragment.get == "results");

    // Test invalid URLs
    assert(!isValidUrl(""));
    assert(!isValidUrl("not-a-url"));

    // Test query parsing
    auto params = parseQueryParams("a=1&b=2&c");
    assert(params["a"] == "1");
    assert(params["b"] == "2");
    assert(params["c"] == "");
}
