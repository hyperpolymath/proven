// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

///
/// SafeString -- Safe string operations via libproven JNA FFI.
///
/// Provides UTF-8 validation and injection-safe escaping for SQL, HTML,
/// and JavaScript contexts. All computation is performed by the Idris 2
/// verified implementation. No escaping logic is reimplemented in Flix.
///
mod Proven.SafeString {

    use Proven.LibProven.getFunction;

    ///
    /// Internal helper: invoke a proven string validation function.
    /// Takes a string, passes pointer and length to the C function.
    /// Returns Option[Bool] (None on error).
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
    /// Internal helper: invoke a proven string escape function.
    /// Takes a string, passes pointer and length to the C function.
    /// Returns the escaped string, or None on error.
    /// The C function allocates a new string which must be freed.
    ///
    def callEscapeString(funcName: String, input: String): Option[String] \ IO =
        region rc {
            let func = getFunction(funcName);
            let bytes = String.getBytes(input);
            import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};
            let ptrObj: ##java.lang.Object = checked_cast(bytes);
            let lenObj: ##java.lang.Object = checked_cast(Integer.valueOf(Array.length(bytes)));
            let args: Array[##java.lang.Object, rc] = Array#{ptrObj, lenObj} @ rc;
            // The StringResult struct contains {status, *ptr, len}.
            // Via JNA, we use invokePointer to get a Pointer to the return value,
            // then read the string from it and free the C allocation.
            import com.sun.jna.Function.invokePointer(Array[##java.lang.Object, rc]): ##com.sun.jna.Pointer \ IO;
            let resultPtr = invokePointer(func, args);
            // Read status from offset 0 (int32)
            import com.sun.jna.Pointer.getInt(Int64): Int32 \ IO;
            let status = getInt(resultPtr, 0i64);
            if (status == 0i32) {
                // Read string pointer from offset 8 (pointer, 8 bytes on 64-bit)
                import com.sun.jna.Pointer.getPointer(Int64): ##com.sun.jna.Pointer \ IO;
                let strPtr = getPointer(resultPtr, 8i64);
                // Read length from offset 16
                import com.sun.jna.Pointer.getLong(Int64): Int64 \ IO;
                let strLen = getLong(resultPtr, 16i64);
                // Read the string bytes
                import com.sun.jna.Pointer.getString(Int64): String \ IO;
                let result = getString(strPtr, 0i64);
                // Free the C-allocated string
                let freeFunc = getFunction("proven_free_string");
                let freeArgs: Array[##java.lang.Object, rc] = Array#{checked_cast(strPtr)} @ rc;
                import com.sun.jna.Function.invoke(Array[##java.lang.Object, rc]): ##java.lang.Object \ IO;
                let _ = invoke(freeFunc, freeArgs);
                Some(result)
            } else {
                None
            }
        }

    ///
    /// Check if a byte sequence is valid UTF-8.
    /// Returns `Some(true)` if valid, `Some(false)` if not, `None` on error.
    ///
    pub def isValidUtf8(input: String): Option[Bool] \ IO =
        callBoolWithString("proven_string_is_valid_utf8", input)

    ///
    /// Escape a string for safe use in SQL queries.
    /// Doubles single quotes. Returns `None` on error.
    ///
    pub def escapeSql(input: String): Option[String] \ IO =
        callEscapeString("proven_string_escape_sql", input)

    ///
    /// Escape a string for safe use in HTML.
    /// Escapes <, >, &, ", and ' characters. Returns `None` on error.
    ///
    pub def escapeHtml(input: String): Option[String] \ IO =
        callEscapeString("proven_string_escape_html", input)

    ///
    /// Escape a string for safe use in JavaScript string literals.
    /// Returns `None` on error.
    ///
    pub def escapeJs(input: String): Option[String] \ IO =
        callEscapeString("proven_string_escape_js", input)

}
