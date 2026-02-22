// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeUrl - URL parsing and validation via libproven FFI.
// All computation is performed in verified Idris 2 code. This module
// provides an idiomatic Haxe wrapper around the raw C FFI calls.

package proven;

import proven.LibProven;

/**
 * Parsed URL components returned by SafeUrl.parse().
 */
typedef UrlComponents = {
    /** URL scheme (e.g., "https") */
    var scheme:String;
    /** Host component (e.g., "example.com") */
    var host:String;
    /** Port number, or null if not specified */
    var port:Null<Int>;
    /** Path component */
    var path:String;
    /** Query string (without leading '?') */
    var query:String;
    /** Fragment (without leading '#') */
    var fragment:String;
};

/**
 * Safe URL parsing and validation.
 *
 * All operations delegate to the formally verified Idris 2 core via
 * libproven. Returns `null` on error. Memory for URL components is
 * freed automatically after extraction.
 */
class SafeUrl {
    /**
     * Parse a URL into its components.
     * @param s URL string
     * @return Parsed URL components, or null on parse failure
     */
    public static function parse(s:String):Null<UrlComponents> {
        return LibProven.urlParse(s);
    }
}
