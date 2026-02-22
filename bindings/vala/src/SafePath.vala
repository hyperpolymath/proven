// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafePath.vala -- Filesystem path traversal prevention.
 *
 * Every method delegates to libproven via C extern calls declared in
 * lib_proven.vapi.  No path logic is reimplemented here.
 */
namespace Proven {

    /**
     * Safe filesystem path operations.
     *
     * Detects directory traversal attacks ("..") and sanitises filenames
     * by stripping dangerous characters.
     */
    public class SafePath : GLib.Object {

        /**
         * Check whether a path contains directory traversal sequences ("..").
         *
         * @param path Path string to check.
         * @return true if traversal is detected, false if safe; null on error.
         */
        public static bool? has_traversal (string path) {
            unowned uint8[] data = (uint8[]) path.data;
            LibProven.BoolResult r = LibProven.path_has_traversal (data);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }

        /**
         * Sanitise a filename by removing dangerous characters.
         *
         * The returned filename is safe to use in filesystem operations.
         *
         * @param filename Raw filename to sanitise.
         * @return Sanitised filename, or null on error.
         */
        public static string? sanitize_filename (string filename) {
            unowned uint8[] data = (uint8[]) filename.data;
            LibProven.StringResult r = LibProven.path_sanitize_filename (data);
            if (r.status != 0) {
                return null;
            }
            string result = ((string) r.value).substring (0, (long) r.length);
            LibProven.free_string (r.value);
            return result;
        }
    }
}
