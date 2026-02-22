// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

///
/// SafeJson -- Safe JSON validation via libproven JNA FFI.
///
/// Validates JSON strings and determines root value types without full parsing.
/// All computation is performed by the Idris 2 verified implementation.
/// No JSON parsing logic is reimplemented in Flix.
///
mod Proven.SafeJson {

    use Proven.LibProven.getFunction;

    ///
    /// JSON value types returned by getType.
    ///
    pub enum JsonType with Eq, Order, ToString {
        case JsonNull
        case JsonBool
        case JsonNumber
        case JsonString
        case JsonArray
        case JsonObject
        case JsonInvalid
    }

    ///
    /// Convert a raw integer to a JsonType.
    ///
    pub def jsonTypeFromInt32(code: Int32): JsonType =
        match code {
            case 0i32  => JsonType.JsonNull
            case 1i32  => JsonType.JsonBool
            case 2i32  => JsonType.JsonNumber
            case 3i32  => JsonType.JsonString
            case 4i32  => JsonType.JsonArray
            case 5i32  => JsonType.JsonObject
            case _     => JsonType.JsonInvalid
        }

    ///
    /// Internal helper: invoke a proven boolean function with a string argument.
    ///
    def callBoolWithString(funcName: String, input: String): Option[Bool] \ IO =
        region rc {
            let func = getFunction(funcName);
            let bytes = String.getBytes(input);
            import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};
            let ptrObj: ##java.lang.Object = checked_cast(bytes);
            let lenObj: ##java.lang.Object = checked_cast(Integer.valueOf(Array.length(bytes)));
            let args: Array[##java.lang.Object, rc] = Array#{ptrObj, lenObj} @ rc;
            import com.sun.jna.Function.invokeLong(Array[##java.lang.Object, rc]): Int64 \ IO;
            let raw = invokeLong(func, args);
            let status = Int64.clampToInt32(raw);
            if (status == 0i32)
                let boolVal = raw >>> 32;
                Some(boolVal != 0i64)
            else
                None
        }

    ///
    /// Check if a string is valid JSON.
    /// Returns `Some(true)` if valid, `Some(false)` if not, `None` on error.
    ///
    pub def isValid(input: String): Option[Bool] \ IO =
        callBoolWithString("proven_json_is_valid", input)

    ///
    /// Get the JSON value type at the root level.
    /// Returns `JsonInvalid` if the input is not valid JSON.
    ///
    pub def getType(input: String): JsonType \ IO =
        region rc {
            let func = getFunction("proven_json_get_type");
            let bytes = String.getBytes(input);
            import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};
            let ptrObj: ##java.lang.Object = checked_cast(bytes);
            let lenObj: ##java.lang.Object = checked_cast(Integer.valueOf(Array.length(bytes)));
            let args: Array[##java.lang.Object, rc] = Array#{ptrObj, lenObj} @ rc;
            import com.sun.jna.Function.invokeInt(Array[##java.lang.Object, rc]): Int32 \ IO;
            let code = invokeInt(func, args);
            jsonTypeFromInt32(code)
        }

    ///
    /// Convenience: check if the input is a JSON object.
    ///
    pub def isObject(input: String): Bool \ IO =
        match getType(input) {
            case JsonType.JsonObject => true
            case _                   => false
        }

    ///
    /// Convenience: check if the input is a JSON array.
    ///
    pub def isArray(input: String): Bool \ IO =
        match getType(input) {
            case JsonType.JsonArray => true
            case _                  => false
        }

}
