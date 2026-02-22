// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeJson.vala -- JSON validation and type detection.
 *
 * Every method delegates to libproven via C extern calls declared in
 * lib_proven.vapi.  No JSON logic is reimplemented here.
 */
namespace Proven {

    /**
     * The type of a JSON value at the root level.
     */
    public enum JsonValueType {
        /** JSON null. */
        NULL,
        /** JSON boolean. */
        BOOL,
        /** JSON number. */
        NUMBER,
        /** JSON string. */
        STRING,
        /** JSON array. */
        ARRAY,
        /** JSON object. */
        OBJECT,
        /** Not valid JSON. */
        INVALID;

        internal static JsonValueType from_c (LibProven.JsonType ctype) {
            switch (ctype) {
                case LibProven.JsonType.NULL:    return NULL;
                case LibProven.JsonType.BOOL:    return BOOL;
                case LibProven.JsonType.NUMBER:  return NUMBER;
                case LibProven.JsonType.STRING:  return STRING;
                case LibProven.JsonType.ARRAY:   return ARRAY;
                case LibProven.JsonType.OBJECT:  return OBJECT;
                default:                         return INVALID;
            }
        }
    }

    /**
     * Safe JSON validation and type detection.
     */
    public class SafeJson : GLib.Object {

        /**
         * Check whether a string is valid JSON.
         *
         * @param json JSON string to validate.
         * @return true if valid JSON; null on internal error.
         */
        public static bool? is_valid (string json) {
            unowned uint8[] data = (uint8[]) json.data;
            LibProven.BoolResult r = LibProven.json_is_valid (data);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }

        /**
         * Get the root-level JSON value type.
         *
         * @param json JSON string.
         * @return The type of the root value, or INVALID if not valid JSON.
         */
        public static JsonValueType get_type (string json) {
            unowned uint8[] data = (uint8[]) json.data;
            LibProven.JsonType t = LibProven.json_get_type (data);
            return JsonValueType.from_c (t);
        }
    }
}
