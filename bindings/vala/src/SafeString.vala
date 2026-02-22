// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeString.vala -- Text escaping and UTF-8 validation.
 *
 * Every method delegates to libproven via C extern calls declared in
 * lib_proven.vapi.  No string logic is reimplemented here.
 */
namespace Proven {

    /**
     * Safe string operations that handle encoding and escaping correctly.
     *
     * Provides UTF-8 validation and context-specific escaping for SQL,
     * HTML, and JavaScript string literals.
     */
    public class SafeString : GLib.Object {

        /**
         * Check whether raw bytes form valid UTF-8.
         *
         * @param data Raw byte data to validate.
         * @return true if valid UTF-8, false otherwise; null on internal error.
         */
        public static bool? is_valid_utf8 (uint8[] data) {
            LibProven.BoolResult r = LibProven.string_is_valid_utf8 (data);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }

        /**
         * Escape a string for safe embedding in SQL (single-quote escaping).
         *
         * Prefer parameterized queries over string escaping where possible.
         *
         * @param input String to escape.
         * @return Escaped string, or null on error.
         */
        public static string? escape_sql (string input) {
            unowned uint8[] data = (uint8[]) input.data;
            LibProven.StringResult r = LibProven.string_escape_sql (data);
            if (r.status != 0) {
                return null;
            }
            string result = ((string) r.value).substring (0, (long) r.length);
            LibProven.free_string (r.value);
            return result;
        }

        /**
         * Escape a string for safe embedding in HTML (prevents XSS).
         *
         * @param input String to escape.
         * @return Escaped string, or null on error.
         */
        public static string? escape_html (string input) {
            unowned uint8[] data = (uint8[]) input.data;
            LibProven.StringResult r = LibProven.string_escape_html (data);
            if (r.status != 0) {
                return null;
            }
            string result = ((string) r.value).substring (0, (long) r.length);
            LibProven.free_string (r.value);
            return result;
        }

        /**
         * Escape a string for safe embedding in JavaScript string literals.
         *
         * @param input String to escape.
         * @return Escaped string, or null on error.
         */
        public static string? escape_js (string input) {
            unowned uint8[] data = (uint8[]) input.data;
            LibProven.StringResult r = LibProven.string_escape_js (data);
            if (r.status != 0) {
                return null;
            }
            string result = ((string) r.value).substring (0, (long) r.length);
            LibProven.free_string (r.value);
            return result;
        }
    }
}
