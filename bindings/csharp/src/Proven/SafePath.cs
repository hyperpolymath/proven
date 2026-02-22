// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafePath.cs - Filesystem operations that prevent directory traversal attacks.
//
// Thin P/Invoke wrapper over libproven. ALL computation is performed in
// verified Idris 2 code via the Zig FFI bridge. No logic is reimplemented here.

namespace Proven
{
    /// <summary>
    /// Safe path operations backed by formally verified Idris 2 code.
    /// Provides directory traversal detection and filename sanitization.
    /// All methods delegate to the libproven FFI. Returns null on error.
    /// </summary>
    public static class SafePath
    {
        /// <summary>
        /// Check if a path contains directory traversal sequences ("..").
        /// Delegates to proven_path_has_traversal via FFI.
        /// </summary>
        /// <param name="path">The path string to check.</param>
        /// <returns>true if traversal detected, false if safe, null on FFI error.</returns>
        public static bool? HasTraversal(string path)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(path);
            return MarshalHelpers.BoolResultToNullable(
                LibProven.proven_path_has_traversal(bytes, (nuint)bytes.Length));
        }

        /// <summary>
        /// Sanitize a filename by removing dangerous characters.
        /// Delegates to proven_path_sanitize_filename via FFI.
        /// </summary>
        /// <param name="filename">The filename to sanitize.</param>
        /// <returns>The sanitized filename, or null on error.</returns>
        public static string? SanitizeFilename(string filename)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(filename);
            return MarshalHelpers.StringResultToManaged(
                LibProven.proven_path_sanitize_filename(bytes, (nuint)bytes.Length));
        }

        /// <summary>
        /// Check if a path is safe (does not contain traversal sequences).
        /// Convenience method that inverts HasTraversal.
        /// Delegates to proven_path_has_traversal via FFI.
        /// </summary>
        /// <param name="path">The path string to check.</param>
        /// <returns>true if the path is safe, false if traversal detected, null on FFI error.</returns>
        public static bool? IsSafe(string path)
        {
            bool? hasTraversal = HasTraversal(path);
            if (hasTraversal is null)
            {
                return null;
            }
            return !hasTraversal.Value;
        }
    }
}
