// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeUrl.vala -- URL parsing and HTTP URL encoding/decoding.
 *
 * Every method delegates to libproven via C extern calls declared in
 * lib_proven.vapi.  No URL logic is reimplemented here.
 */
namespace Proven {

    /**
     * Parsed URL components returned by {@link SafeUrl.parse}.
     */
    public class UrlParts : GLib.Object {
        /** URL scheme (e.g. "https"). */
        public string scheme   { get; private set; }
        /** Hostname. */
        public string host     { get; private set; }
        /** Port number, or -1 if not specified. */
        public int    port     { get; private set; }
        /** Path component. */
        public string path     { get; private set; }
        /** Query string (without leading '?'). */
        public string query    { get; private set; }
        /** Fragment (without leading '#'). */
        public string fragment { get; private set; }

        internal UrlParts (string scheme, string host, int port,
                           string path, string query, string fragment) {
            this.scheme   = scheme;
            this.host     = host;
            this.port     = port;
            this.path     = path;
            this.query    = query;
            this.fragment = fragment;
        }
    }

    /**
     * Safe URL parsing, validation, and encoding.
     */
    public class SafeUrl : GLib.Object {

        /**
         * Parse a URL into its component parts.
         *
         * @param url URL string to parse.
         * @return Parsed URL components, or null on parse failure.
         */
        public static UrlParts? parse (string url) {
            unowned uint8[] data = (uint8[]) url.data;
            LibProven.UrlResult r = LibProven.url_parse (data);
            if (r.status != 0) {
                return null;
            }

            string scheme   = r.components.scheme != null
                ? ((string) r.components.scheme).substring (0, (long) r.components.scheme_len)
                : "";
            string host     = r.components.host != null
                ? ((string) r.components.host).substring (0, (long) r.components.host_len)
                : "";
            int    port     = r.components.has_port ? (int) r.components.port : -1;
            string path_str = r.components.path != null
                ? ((string) r.components.path).substring (0, (long) r.components.path_len)
                : "";
            string query    = r.components.query != null
                ? ((string) r.components.query).substring (0, (long) r.components.query_len)
                : "";
            string fragment = r.components.fragment != null
                ? ((string) r.components.fragment).substring (0, (long) r.components.fragment_len)
                : "";

            LibProven.url_free (&r.components);

            return new UrlParts (scheme, host, port, path_str, query, fragment);
        }

        /**
         * URL-encode a string (RFC 3986 percent encoding).
         *
         * Unreserved characters (A-Za-z0-9-._~) pass through; all others
         * become percent-encoded (%XX).
         *
         * @param input String to encode.
         * @return Encoded string, or null on error.
         */
        public static string? url_encode (string input) {
            unowned uint8[] data = (uint8[]) input.data;
            LibProven.StringResult r = LibProven.http_url_encode (data);
            if (r.status != 0) {
                return null;
            }
            string result = ((string) r.value).substring (0, (long) r.length);
            LibProven.free_string (r.value);
            return result;
        }

        /**
         * URL-decode a percent-encoded string.
         *
         * @param input Encoded string.
         * @return Decoded string, or null on error.
         */
        public static string? url_decode (string input) {
            unowned uint8[] data = (uint8[]) input.data;
            LibProven.StringResult r = LibProven.http_url_decode (data);
            if (r.status != 0) {
                return null;
            }
            string result = ((string) r.value).substring (0, (long) r.length);
            LibProven.free_string (r.value);
            return result;
        }
    }
}
