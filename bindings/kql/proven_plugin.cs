// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// proven_plugin.cs - C# plugin for Azure Data Explorer calling libproven via P/Invoke
//
// This plugin exposes formally verified functions from libproven as
// Azure Data Explorer inline C# functions. ALL computation is performed
// in Idris 2 via the Zig FFI bridge. This file contains ONLY P/Invoke
// marshaling. No algorithms are reimplemented here.
//
// Deployment:
//   1. Enable the C# sandbox plugin on the ADX cluster
//   2. Deploy libproven.so to the cluster sandbox image
//   3. Register functions via .create-or-alter commands

using System;
using System.Runtime.InteropServices;
using System.Text;

namespace Proven.Kql
{
    // ========================================================================
    // P/Invoke declarations for libproven
    // ========================================================================

    /// <summary>
    /// P/Invoke bindings to the libproven C ABI.
    /// All computation is delegated to verified Idris 2 code.
    /// </summary>
    internal static class LibProven
    {
        private const string LibName = "proven";

        // Result structs matching C ABI layout

        [StructLayout(LayoutKind.Sequential)]
        public struct IntResult
        {
            public int Status;
            public long Value;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct BoolResult
        {
            public int Status;
            [MarshalAs(UnmanagedType.U1)]
            public bool Value;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct StringResult
        {
            public int Status;
            public IntPtr Ptr;
            public nuint Length;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct IPv4Result
        {
            public int Status;
            [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
            public byte[] Octets;
        }

        // Lifecycle
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern int proven_init();

        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern void proven_deinit();

        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern void proven_free_string(IntPtr ptr);

        // SafeMath
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_math_add_checked(long a, long b);

        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_math_sub_checked(long a, long b);

        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_math_mul_checked(long a, long b);

        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_math_div(long numerator, long denominator);

        // SafeEmail
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_email_is_valid(byte[] ptr, nuint len);

        // SafeUrl
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr proven_url_parse(byte[] ptr, nuint len);

        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern void proven_url_free(IntPtr components);

        // SafeNetwork
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IPv4Result proven_network_parse_ipv4(byte[] ptr, nuint len);

        // SafeJson
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern BoolResult proven_json_is_valid(byte[] ptr, nuint len);

        // SafeString
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_string_escape_html(byte[] ptr, nuint len);

        // SafeChecksum
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntResult proven_checksum_crc32(byte[] ptr, nuint len);

        // SafeHex
        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern StringResult proven_hex_encode(
            byte[] ptr, nuint len,
            [MarshalAs(UnmanagedType.U1)] bool uppercase);

        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr proven_hex_decode(byte[] ptr, nuint len);

        [DllImport(LibName, CallingConvention = CallingConvention.Cdecl)]
        public static extern void proven_hex_free(IntPtr result);
    }

    // ========================================================================
    // Proven KQL plugin functions
    // ========================================================================

    /// <summary>
    /// Proven-backed functions for use as Azure Data Explorer C# inline plugins.
    /// Each method corresponds to a KQL function that can be invoked via
    /// the evaluate csharp() plugin.
    ///
    /// All functions return null on error (overflow, invalid input, etc.).
    /// </summary>
    public static class ProvenPlugin
    {
        private static bool _initialized = false;

        /// <summary>
        /// Ensure the proven runtime is initialized exactly once.
        /// </summary>
        private static void EnsureInit()
        {
            if (!_initialized)
            {
                int rc = LibProven.proven_init();
                if (rc == 0)
                    _initialized = true;
            }
        }

        /// <summary>
        /// Extract a managed string from a StringResult, freeing native memory.
        /// Returns null if the status is not OK.
        /// </summary>
        private static string? ExtractString(LibProven.StringResult result)
        {
            if (result.Status == 0 && result.Ptr != IntPtr.Zero)
            {
                try
                {
                    string managed = Marshal.PtrToStringUTF8(result.Ptr, (int)result.Length)
                        ?? string.Empty;
                    return managed;
                }
                finally
                {
                    LibProven.proven_free_string(result.Ptr);
                }
            }

            if (result.Ptr != IntPtr.Zero)
                LibProven.proven_free_string(result.Ptr);

            return null;
        }

        // ====================================================================
        // SafeMath
        // ====================================================================

        /// <summary>Checked addition. Returns null on overflow.</summary>
        public static long? SafeAdd(long a, long b)
        {
            EnsureInit();
            var result = LibProven.proven_math_add_checked(a, b);
            return result.Status == 0 ? result.Value : null;
        }

        /// <summary>Checked subtraction. Returns null on underflow.</summary>
        public static long? SafeSub(long a, long b)
        {
            EnsureInit();
            var result = LibProven.proven_math_sub_checked(a, b);
            return result.Status == 0 ? result.Value : null;
        }

        /// <summary>Checked multiplication. Returns null on overflow.</summary>
        public static long? SafeMul(long a, long b)
        {
            EnsureInit();
            var result = LibProven.proven_math_mul_checked(a, b);
            return result.Status == 0 ? result.Value : null;
        }

        /// <summary>Safe division. Returns null on division by zero.</summary>
        public static long? SafeDiv(long a, long b)
        {
            EnsureInit();
            var result = LibProven.proven_math_div(a, b);
            return result.Status == 0 ? result.Value : null;
        }

        // ====================================================================
        // Validation
        // ====================================================================

        /// <summary>Validate email address (RFC 5321).</summary>
        public static bool? ValidateEmail(string addr)
        {
            if (addr == null) return null;
            EnsureInit();
            byte[] bytes = Encoding.UTF8.GetBytes(addr);
            var result = LibProven.proven_email_is_valid(bytes, (nuint)bytes.Length);
            return result.Status == 0 ? result.Value : null;
        }

        /// <summary>Validate URL by attempting to parse it.</summary>
        public static bool? ValidateUrl(string url)
        {
            if (url == null) return null;
            EnsureInit();
            byte[] bytes = Encoding.UTF8.GetBytes(url);
            IntPtr resultPtr = LibProven.proven_url_parse(bytes, (nuint)bytes.Length);
            if (resultPtr == IntPtr.Zero)
                return false;
            int status = Marshal.ReadInt32(resultPtr);
            if (status == 0)
            {
                // Free the URL components at the correct offset
                IntPtr componentsPtr = resultPtr + 4; // offset past status
                LibProven.proven_url_free(componentsPtr);
            }
            return status == 0;
        }

        /// <summary>Validate IPv4 address string.</summary>
        public static bool? ValidateIpv4(string addr)
        {
            if (addr == null) return null;
            EnsureInit();
            byte[] bytes = Encoding.UTF8.GetBytes(addr);
            var result = LibProven.proven_network_parse_ipv4(bytes, (nuint)bytes.Length);
            return result.Status == 0;
        }

        /// <summary>Validate JSON string.</summary>
        public static bool? ValidateJson(string doc)
        {
            if (doc == null) return null;
            EnsureInit();
            byte[] bytes = Encoding.UTF8.GetBytes(doc);
            var result = LibProven.proven_json_is_valid(bytes, (nuint)bytes.Length);
            return result.Status == 0 ? result.Value : null;
        }

        // ====================================================================
        // String operations
        // ====================================================================

        /// <summary>Sanitize string by escaping HTML entities.</summary>
        public static string? SanitizeString(string input)
        {
            if (input == null) return null;
            EnsureInit();
            byte[] bytes = Encoding.UTF8.GetBytes(input);
            var result = LibProven.proven_string_escape_html(bytes, (nuint)bytes.Length);
            return ExtractString(result);
        }

        /// <summary>CRC32 checksum as hex string.</summary>
        public static string? HashSha256(string input)
        {
            if (input == null) return null;
            EnsureInit();
            byte[] bytes = Encoding.UTF8.GetBytes(input);
            var crcResult = LibProven.proven_checksum_crc32(bytes, (nuint)bytes.Length);
            if (crcResult.Status != 0) return null;

            uint crc = (uint)crcResult.Value;
            byte[] crcBytes = new byte[] {
                (byte)((crc >> 24) & 0xFF),
                (byte)((crc >> 16) & 0xFF),
                (byte)((crc >> 8) & 0xFF),
                (byte)(crc & 0xFF)
            };

            var hexResult = LibProven.proven_hex_encode(crcBytes, 4, false);
            return ExtractString(hexResult);
        }

        /// <summary>Hex-encode input bytes.</summary>
        public static string? HexEncode(string input)
        {
            if (input == null) return null;
            EnsureInit();
            byte[] bytes = Encoding.UTF8.GetBytes(input);
            var result = LibProven.proven_hex_encode(bytes, (nuint)bytes.Length, false);
            return ExtractString(result);
        }

        /// <summary>Hex-decode input string.</summary>
        public static string? HexDecode(string input)
        {
            if (input == null) return null;
            EnsureInit();
            byte[] bytes = Encoding.UTF8.GetBytes(input);
            IntPtr resultPtr = LibProven.proven_hex_decode(bytes, (nuint)bytes.Length);
            if (resultPtr == IntPtr.Zero) return null;

            int status = Marshal.ReadInt32(resultPtr);
            if (status != 0)
            {
                LibProven.proven_hex_free(resultPtr);
                return null;
            }

            IntPtr dataPtr = Marshal.ReadIntPtr(resultPtr, 8);
            long dataLen = Marshal.ReadInt64(resultPtr, 16);

            if (dataPtr == IntPtr.Zero || dataLen <= 0)
            {
                LibProven.proven_hex_free(resultPtr);
                return null;
            }

            byte[] decoded = new byte[dataLen];
            Marshal.Copy(dataPtr, decoded, 0, (int)dataLen);
            LibProven.proven_hex_free(resultPtr);

            return Encoding.UTF8.GetString(decoded);
        }
    }
}
