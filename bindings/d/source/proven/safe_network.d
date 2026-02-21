// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe network validation and operations.
 */
module proven.safe_network;

import std.string : toLower, indexOf, split;
import std.conv : to, ConvException;
import std.algorithm : all, startsWith, canFind;
import std.typecons : Nullable, nullable;
import std.regex : regex, matchAll;

/// IP address classification.
enum IpClassification
{
    loopback,
    private_,
    reserved,
    public_,
    invalid
}

/// Parsed IPv4 address.
struct IPv4Address
{
    ubyte a, b, c, d;

    string toString() const pure @safe
    {
        import std.format : format;

        return format!"%d.%d.%d.%d"(a, b, c, d);
    }

    uint toInt() const pure nothrow @safe @nogc
    {
        return (cast(uint) a << 24) | (cast(uint) b << 16) | (cast(uint) c << 8) | cast(uint) d;
    }
}

/// Parse IPv4 address string.
Nullable!IPv4Address parseIPv4(string address) pure @safe
{
    auto parts = address.split('.');
    if (parts.length != 4)
        return Nullable!IPv4Address.init;

    ubyte[4] octets;
    foreach (i, part; parts)
    {
        if (part.length == 0)
            return Nullable!IPv4Address.init;
        // Check for leading zeros
        if (part.length > 1 && part[0] == '0')
            return Nullable!IPv4Address.init;
        // Check all digits
        foreach (c; part)
        {
            if (c < '0' || c > '9')
                return Nullable!IPv4Address.init;
        }
        try
        {
            auto value = part.to!int;
            if (value < 0 || value > 255)
                return Nullable!IPv4Address.init;
            octets[i] = cast(ubyte) value;
        }
        catch (ConvException)
        {
            return Nullable!IPv4Address.init;
        }
    }

    return nullable(IPv4Address(octets[0], octets[1], octets[2], octets[3]));
}

/// Check if IPv4 is a loopback address.
bool isLoopback(IPv4Address ip) pure nothrow @safe @nogc
{
    return ip.a == 127;
}

/// Check if IPv4 is a private address (RFC 1918).
bool isPrivateIP(IPv4Address ip) pure nothrow @safe @nogc
{
    return ip.a == 10 || (ip.a == 172 && ip.b >= 16 && ip.b <= 31) || (ip.a == 192 && ip.b == 168);
}

/// Check if IPv4 is a reserved address.
bool isReserved(IPv4Address ip) pure nothrow @safe @nogc
{
    return ip.a == 0 || (ip.a == 100 && ip.b >= 64 && ip.b <= 127) || (ip.a == 169 && ip.b == 254) ||
        (ip.a == 192 && ip.b == 0 && ip.c == 0) || (ip.a == 192 && ip.b == 0 && ip.c == 2) ||
        (ip.a == 198 && ip.b == 51 && ip.c == 100) || (ip.a == 203 && ip.b == 0 && ip.c == 113) ||
        (ip.a >= 224 && ip.a <= 239) || ip.a >= 240;
}

/// Check if IPv4 is a public address.
bool isPublicIP(IPv4Address ip) pure nothrow @safe @nogc
{
    return !isLoopback(ip) && !isPrivateIP(ip) && !isReserved(ip);
}

/// Classify an IPv4 address.
IpClassification classifyIP(IPv4Address ip) pure nothrow @safe @nogc
{
    if (isLoopback(ip))
        return IpClassification.loopback;
    if (isPrivateIP(ip))
        return IpClassification.private_;
    if (isReserved(ip))
        return IpClassification.reserved;
    return IpClassification.public_;
}

/// Classify IPv4 from string.
IpClassification classifyIPString(string address) pure @safe
{
    auto ip = parseIPv4(address);
    if (ip.isNull)
        return IpClassification.invalid;
    return classifyIP(ip.get);
}

/// Check if IP is in CIDR range.
bool ipInRange(IPv4Address ip, IPv4Address network, int prefixLength) pure nothrow @safe @nogc
{
    if (prefixLength < 0 || prefixLength > 32)
        return false;

    uint mask = prefixLength == 0 ? 0 : 0xFFFFFFFF << (32 - prefixLength);
    return (ip.toInt() & mask) == (network.toInt() & mask);
}

/// Check if string is valid IPv4.
bool isValidIPv4(string address) pure @safe
{
    return !parseIPv4(address).isNull;
}

/// Check if port number is valid.
bool isValidPort(int port) pure nothrow @safe @nogc
{
    return port >= 1 && port <= 65535;
}

/// Check if port is privileged.
bool isPrivilegedPort(int port) pure nothrow @safe @nogc
{
    return port >= 1 && port < 1024;
}

/// Check if hostname is valid.
bool isValidHostname(string hostname) pure @safe
{
    if (hostname.length == 0 || hostname.length > 253)
        return false;

    auto labels = hostname.split('.');
    foreach (label; labels)
    {
        if (label.length == 0 || label.length > 63)
            return false;
        // Must start and end with alphanumeric
        if (!isAlphaNum(label[0]))
            return false;
        if (!isAlphaNum(label[$ - 1]))
            return false;
        // Middle can include hyphens
        foreach (c; label)
        {
            if (!isAlphaNum(c) && c != '-')
                return false;
        }
    }
    return true;
}

/// Check if character is alphanumeric.
private bool isAlphaNum(char c) pure nothrow @safe @nogc
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9');
}

/// Check if URL is valid (basic check).
bool isValidUrl(string url) pure @safe
{
    return url.startsWith("http://") || url.startsWith("https://");
}

/// Check if URL host is a private IP (SSRF protection).
bool isPrivateUrl(string url) @safe
{
    // Extract host from URL
    string host = url;

    // Remove protocol
    if (host.startsWith("http://"))
        host = host[7 .. $];
    else if (host.startsWith("https://"))
        host = host[8 .. $];

    // Remove path
    auto slashIdx = host.indexOf('/');
    if (slashIdx >= 0)
        host = host[0 .. slashIdx];

    // Remove port
    auto colonIdx = host.indexOf(':');
    if (colonIdx >= 0)
        host = host[0 .. colonIdx];

    // Check common private hosts
    if (host == "localhost" || host == "127.0.0.1" || host == "::1")
        return true;

    // Check if it's a private IP
    auto ip = parseIPv4(host);
    if (ip.isNull)
        return false;
    return isPrivateIP(ip.get) || isLoopback(ip.get) || isReserved(ip.get);
}

// Unit tests
unittest
{
    assert(!parseIPv4("192.168.1.1").isNull);
    auto ip = parseIPv4("192.168.1.1").get;
    assert(isPrivateIP(ip));
    assert(classifyIP(ip) == IpClassification.private_);
    assert(isValidPort(8080));
}
