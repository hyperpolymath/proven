// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeUrl.cs - URL parsing and validation.
//
// Thin P/Invoke wrapper over libproven. ALL computation is performed in
// verified Idris 2 code via the Zig FFI bridge. No logic is reimplemented here.

using System;
using System.Runtime.InteropServices;

namespace Proven
{
    /// <summary>
    /// Parsed URL components returned by <see cref="SafeUrl.Parse"/>.
    /// Matches the C ABI ProvenUrlComponents layout.
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    internal struct NativeUrlComponents
    {
        public IntPtr Scheme;
        public nuint SchemeLen;
        public IntPtr Host;
        public nuint HostLen;
        public ushort Port;
        [MarshalAs(UnmanagedType.U1)]
        public bool HasPort;
        public IntPtr Path;
        public nuint PathLen;
        public IntPtr Query;
        public nuint QueryLen;
        public IntPtr Fragment;
        public nuint FragmentLen;
    }

    /// <summary>
    /// Native URL result struct matching C ABI ProvenUrlResult.
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    internal struct NativeUrlResult
    {
        public int Status;
        public NativeUrlComponents Components;
    }

    /// <summary>
    /// Managed representation of parsed URL components.
    /// </summary>
    public sealed class ParsedUrl
    {
        /// <summary>URL scheme (e.g. "https").</summary>
        public string Scheme { get; }
        /// <summary>Host component.</summary>
        public string Host { get; }
        /// <summary>Port number, or null if not specified.</summary>
        public ushort? Port { get; }
        /// <summary>Path component.</summary>
        public string Path { get; }
        /// <summary>Query string, or null if not present.</summary>
        public string? Query { get; }
        /// <summary>Fragment identifier, or null if not present.</summary>
        public string? Fragment { get; }

        internal ParsedUrl(string scheme, string host, ushort? port,
                           string path, string? query, string? fragment)
        {
            Scheme = scheme;
            Host = host;
            Port = port;
            Path = path;
            Query = query;
            Fragment = fragment;
        }
    }

    /// <summary>
    /// Safe URL operations backed by formally verified Idris 2 code.
    /// Provides URL parsing and validation. All methods delegate to the
    /// libproven FFI.
    /// </summary>
    public static class SafeUrl
    {
        /// <summary>
        /// Parse a URL string into its components.
        /// Delegates to proven_url_parse via FFI. Frees native resources after
        /// copying data into managed memory.
        /// </summary>
        /// <param name="url">The URL string to parse.</param>
        /// <returns>Parsed URL components, or null on parse failure.</returns>
        public static ParsedUrl? Parse(string url)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(url);
            IntPtr resultPtr = LibProven.proven_url_parse(bytes, (nuint)bytes.Length);

            if (resultPtr == IntPtr.Zero)
            {
                return null;
            }

            try
            {
                var result = Marshal.PtrToStructure<NativeUrlResult>(resultPtr);

                if (result.Status != 0)
                {
                    return null;
                }

                var components = result.Components;

                string scheme = components.Scheme != IntPtr.Zero
                    ? Marshal.PtrToStringUTF8(components.Scheme, (int)components.SchemeLen) ?? ""
                    : "";

                string host = components.Host != IntPtr.Zero
                    ? Marshal.PtrToStringUTF8(components.Host, (int)components.HostLen) ?? ""
                    : "";

                ushort? port = components.HasPort ? components.Port : null;

                string path = components.Path != IntPtr.Zero
                    ? Marshal.PtrToStringUTF8(components.Path, (int)components.PathLen) ?? ""
                    : "";

                string? query = components.Query != IntPtr.Zero && components.QueryLen > 0
                    ? Marshal.PtrToStringUTF8(components.Query, (int)components.QueryLen)
                    : null;

                string? fragment = components.Fragment != IntPtr.Zero && components.FragmentLen > 0
                    ? Marshal.PtrToStringUTF8(components.Fragment, (int)components.FragmentLen)
                    : null;

                return new ParsedUrl(scheme, host, port, path, query, fragment);
            }
            finally
            {
                LibProven.proven_url_free(resultPtr);
            }
        }

        /// <summary>
        /// Check if a URL string is valid (can be parsed).
        /// Delegates to proven_url_parse via FFI.
        /// </summary>
        /// <param name="url">The URL string to validate.</param>
        /// <returns>true if the URL is valid and can be parsed.</returns>
        public static bool IsValid(string url)
        {
            return Parse(url) is not null;
        }

        /// <summary>
        /// Extract the host component from a URL.
        /// Delegates to proven_url_parse via FFI.
        /// </summary>
        /// <param name="url">The URL string.</param>
        /// <returns>The host component, or null on parse failure.</returns>
        public static string? GetHost(string url)
        {
            return Parse(url)?.Host;
        }

        /// <summary>
        /// Extract the scheme component from a URL.
        /// Delegates to proven_url_parse via FFI.
        /// </summary>
        /// <param name="url">The URL string.</param>
        /// <returns>The scheme component, or null on parse failure.</returns>
        public static string? GetScheme(string url)
        {
            return Parse(url)?.Scheme;
        }

        /// <summary>
        /// Check if a URL uses the HTTPS scheme.
        /// Delegates to proven_url_parse via FFI.
        /// </summary>
        /// <param name="url">The URL string.</param>
        /// <returns>true if the scheme is "https".</returns>
        public static bool IsHttps(string url)
        {
            string? scheme = GetScheme(url);
            return string.Equals(scheme, "https", StringComparison.OrdinalIgnoreCase);
        }
    }
}
