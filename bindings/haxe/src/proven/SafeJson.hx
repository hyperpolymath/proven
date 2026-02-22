// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeJson - JSON validation and type detection via libproven FFI.
// All computation is performed in verified Idris 2 code. This module
// provides an idiomatic Haxe wrapper around the raw C FFI calls.

package proven;

import proven.LibProven;

/**
 * JSON value types returned by SafeJson.getType().
 */
enum abstract JsonType(Int) from Int to Int {
    var Null = 0;
    var Bool = 1;
    var Number = 2;
    var JsonString = 3;
    var Array = 4;
    var Object = 5;
    var Invalid = -1;

    /**
     * Returns a human-readable description of the JSON type.
     */
    public function toString():String {
        return switch (cast this : Int) {
            case 0: "null";
            case 1: "boolean";
            case 2: "number";
            case 3: "string";
            case 4: "array";
            case 5: "object";
            default: "invalid";
        };
    }
}

/**
 * Safe JSON validation and type detection.
 *
 * All operations delegate to the formally verified Idris 2 core via
 * libproven. Returns `null` on error.
 */
class SafeJson {
    /**
     * Check if string is valid JSON.
     * @param s JSON string
     * @return true if valid JSON, false if invalid, null on error
     */
    public static function isValid(s:String):Null<Bool> {
        return LibProven.jsonIsValid(s);
    }

    /**
     * Get the JSON value type at the root level.
     * @param s JSON string
     * @return JSON type enum value
     */
    public static function getType(s:String):JsonType {
        return LibProven.jsonGetType(s);
    }
}
