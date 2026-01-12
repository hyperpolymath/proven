// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe path operations for directory traversal prevention.
 */
module proven.safe_path;

import std.path : buildPath, baseName, dirName, absolutePath, extension;
import std.string : toLower, replace, strip;
import std.array : appender;
import std.algorithm : canFind, startsWith;
import std.typecons : Nullable, nullable;

/// Result type for path operations.
struct PathResult
{
    string path;
    string error;
    bool ok;

    static PathResult success(string path)
    {
        return PathResult(path, "", true);
    }

    static PathResult failure(string error)
    {
        return PathResult("", error, false);
    }
}

/// Dangerous path patterns.
private immutable string[] traversalPatterns = [
    "..", "./", ".\\", "%2e%2e", "%2e.", ".%2e", "%00"
];

/// Check if path contains traversal sequences.
bool hasTraversal(string path) pure @safe
{
    auto normalized = path.toLower();
    foreach (pattern; traversalPatterns)
    {
        if (normalized.canFind(pattern))
            return true;
    }
    return false;
}

/// Sanitize a filename.
string sanitizeFilename(string filename) pure @safe
{
    string result = filename;

    // Remove directory separators
    result = result.replace("/", "_");
    result = result.replace("\\", "_");

    // Remove null bytes
    result = result.replace("\0", "");

    // Remove leading dots
    while (result.length > 0 && result[0] == '.')
        result = result[1 .. $];

    // Replace dangerous chars
    foreach (c; "<>:\"|?*")
    {
        import std.conv : to;

        result = result.replace(to!string(c), "_");
    }

    // Collapse multiple underscores
    while (result.canFind("__"))
        result = result.replace("__", "_");

    // Trim underscores
    while (result.length > 0 && result[0] == '_')
        result = result[1 .. $];
    while (result.length > 0 && result[$ - 1] == '_')
        result = result[0 .. $ - 1];

    return result.length == 0 ? "unnamed" : result;
}

/// Join paths safely.
PathResult pathJoin(string base, string[] components...) pure @safe
{
    foreach (component; components)
    {
        if (hasTraversal(component))
            return PathResult.failure("Path traversal detected in component: " ~ component);
    }

    string result = base;
    foreach (component; components)
    {
        // Remove leading slashes
        string clean = component;
        while (clean.length > 0 && (clean[0] == '/' || clean[0] == '\\'))
            clean = clean[1 .. $];
        result = buildPath(result, clean);
    }

    return PathResult.success(result);
}

/// Resolve path within base directory.
PathResult resolveWithin(string base, string path) @safe
{
    if (hasTraversal(path))
        return PathResult.failure("Path traversal detected");

    // Get absolute paths
    string absBase = absolutePath(base);

    // Clean the path
    string cleanPath = path;
    while (cleanPath.length > 0 && (cleanPath[0] == '/' || cleanPath[0] == '\\'))
        cleanPath = cleanPath[1 .. $];

    string fullPath = absolutePath(buildPath(absBase, cleanPath));

    // Verify it's within base
    if (!fullPath.startsWith(absBase))
        return PathResult.failure("Path escapes base directory");

    return PathResult.success(fullPath);
}

/// Get file extension safely.
Nullable!string getExtension(string path) pure @safe
{
    if (path.length == 0)
        return Nullable!string.init;

    auto base = baseName(path);
    if (base.length == 0 || base[0] == '.')
        return Nullable!string.init;

    auto ext = extension(path);
    if (ext.length == 0)
        return Nullable!string.init;

    return nullable(ext);
}

/// Check if extension is allowed.
bool extensionAllowed(string path, string[] allowed) pure @safe
{
    auto ext = getExtension(path);
    if (ext.isNull)
        return false;

    auto extLower = ext.get.toLower();
    foreach (a; allowed)
    {
        if (a.toLower() == extLower)
            return true;
    }
    return false;
}

/// Normalize path separators to forward slash.
string normalizeSeparators(string path) pure @safe
{
    return path.replace("\\", "/");
}

/// Check if path is absolute.
bool isAbsolute(string path) pure nothrow @safe @nogc
{
    if (path.length == 0)
        return false;
    return path[0] == '/' || (path.length >= 2 && path[1] == ':');
}

/// Check if path is relative.
bool isRelative(string path) pure nothrow @safe @nogc
{
    return !isAbsolute(path);
}

/// Get parent directory.
Nullable!string parent(string path) pure @safe
{
    if (path.length == 0)
        return Nullable!string.init;
    return nullable(dirName(path));
}

/// Get filename from path.
string filename(string path) pure @safe
{
    return baseName(path);
}

/// Check if filename is hidden (starts with dot).
bool isHidden(string path) pure @safe
{
    auto base = baseName(path);
    return base.length > 0 && base[0] == '.';
}

// Unit tests
unittest
{
    assert(hasTraversal("../../../etc/passwd"));
    assert(!hasTraversal("/var/www/file.txt"));
    assert(sanitizeFilename("../../../etc/passwd") == "etc_passwd");
}
