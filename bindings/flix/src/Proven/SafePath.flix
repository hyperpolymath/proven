// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

///
/// SafePath -- Safe filesystem path operations via libproven JNA FFI.
///
/// Provides directory traversal detection and filename sanitization.
/// All computation is performed by the Idris 2 verified implementation.
/// No path validation logic is reimplemented in Flix.
///
mod Proven.SafePath {

    use Proven.LibProven.getFunction;

    ///
    /// Internal helper: invoke a proven path boolean function.
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
    /// Internal helper: invoke a proven path string function.
    ///
    def callStringWithString(funcName: String, input: String): Option[String] \ IO =
        region rc {
            let func = getFunction(funcName);
            let bytes = String.getBytes(input);
            import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};
            let ptrObj: ##java.lang.Object = checked_cast(bytes);
            let lenObj: ##java.lang.Object = checked_cast(Integer.valueOf(Array.length(bytes)));
            let args: Array[##java.lang.Object, rc] = Array#{ptrObj, lenObj} @ rc;
            import com.sun.jna.Function.invokePointer(Array[##java.lang.Object, rc]): ##com.sun.jna.Pointer \ IO;
            let resultPtr = invokePointer(func, args);
            import com.sun.jna.Pointer.getInt(Int64): Int32 \ IO;
            let status = getInt(resultPtr, 0i64);
            if (status == 0i32) {
                import com.sun.jna.Pointer.getPointer(Int64): ##com.sun.jna.Pointer \ IO;
                let strPtr = getPointer(resultPtr, 8i64);
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
    /// Check if a path contains directory traversal sequences ("..").
    /// Returns `Some(true)` if traversal detected, `Some(false)` if safe,
    /// `None` on error.
    ///
    pub def hasTraversal(path: String): Option[Bool] \ IO =
        callBoolWithString("proven_path_has_traversal", path)

    ///
    /// Sanitize a filename by removing dangerous characters.
    /// Strips path separators, "..", null bytes, and control characters.
    /// Returns `None` on error.
    ///
    pub def sanitizeFilename(filename: String): Option[String] \ IO =
        callStringWithString("proven_path_sanitize_filename", filename)

    ///
    /// Check if a path is safe (no traversal detected).
    /// Returns `Some(true)` if safe, `Some(false)` if traversal detected,
    /// `None` on error.
    ///
    pub def isSafePath(path: String): Option[Bool] \ IO =
        match hasTraversal(path) {
            case Some(t) => Some(not t)
            case None    => None
        }

}
