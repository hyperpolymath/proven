// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

///
/// SafeCrypto -- Safe cryptographic operations via libproven JNA FFI.
///
/// Provides constant-time comparison, secure random bytes, and hex encoding.
/// All computation is performed by the Idris 2 verified implementation.
/// No cryptographic logic is reimplemented in Flix.
///
mod Proven.SafeCrypto {

    use Proven.LibProven.getFunction;

    ///
    /// Constant-time comparison of two strings (timing-attack resistant).
    /// Returns `Some(true)` if equal, `Some(false)` if not, `None` on error.
    ///
    pub def constantTimeEq(a: String, b: String): Option[Bool] \ IO =
        region rc {
            let func = getFunction("proven_crypto_constant_time_eq");
            let aBytes = String.getBytes(a);
            let bBytes = String.getBytes(b);
            import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};
            let aPtr: ##java.lang.Object = checked_cast(aBytes);
            let aLen: ##java.lang.Object = checked_cast(Integer.valueOf(Array.length(aBytes)));
            let bPtr: ##java.lang.Object = checked_cast(bBytes);
            let bLen: ##java.lang.Object = checked_cast(Integer.valueOf(Array.length(bBytes)));
            let args: Array[##java.lang.Object, rc] = Array#{aPtr, aLen, bPtr, bLen} @ rc;
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
    /// Encode a string as lowercase hexadecimal.
    /// Returns `None` on error.
    ///
    pub def hexEncode(input: String): Option[String] \ IO =
        region rc {
            let func = getFunction("proven_hex_encode");
            let bytes = String.getBytes(input);
            import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};
            let ptrObj: ##java.lang.Object = checked_cast(bytes);
            let lenObj: ##java.lang.Object = checked_cast(Integer.valueOf(Array.length(bytes)));
            let falseObj: ##java.lang.Object = checked_cast(Integer.valueOf(0i32));
            let args: Array[##java.lang.Object, rc] = Array#{ptrObj, lenObj, falseObj} @ rc;
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
    /// Encode a string as uppercase hexadecimal.
    /// Returns `None` on error.
    ///
    pub def hexEncodeUpper(input: String): Option[String] \ IO =
        region rc {
            let func = getFunction("proven_hex_encode");
            let bytes = String.getBytes(input);
            import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};
            let ptrObj: ##java.lang.Object = checked_cast(bytes);
            let lenObj: ##java.lang.Object = checked_cast(Integer.valueOf(Array.length(bytes)));
            let trueObj: ##java.lang.Object = checked_cast(Integer.valueOf(1i32));
            let args: Array[##java.lang.Object, rc] = Array#{ptrObj, lenObj, trueObj} @ rc;
            import com.sun.jna.Function.invokePointer(Array[##java.lang.Object, rc]): ##com.sun.jna.Pointer \ IO;
            let resultPtr = invokePointer(func, args);
            import com.sun.jna.Pointer.getInt(Int64): Int32 \ IO;
            let status = getInt(resultPtr, 0i64);
            if (status == 0i32) {
                import com.sun.jna.Pointer.getPointer(Int64): ##com.sun.jna.Pointer \ IO;
                let strPtr = getPointer(resultPtr, 8i64);
                import com.sun.jna.Pointer.getString(Int64): String \ IO;
                let result = getString(strPtr, 0i64);
                let freeFunc = getFunction("proven_free_string");
                let freeArgs: Array[##java.lang.Object, rc] = Array#{checked_cast(strPtr)} @ rc;
                import com.sun.jna.Function.invoke(Array[##java.lang.Object, rc]): ##java.lang.Object \ IO;
                let _ = invoke(freeFunc, freeArgs);
                Some(result)
            } else {
                None
            }
        }

}
