// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

///
/// SafeEmail -- Safe email validation via libproven JNA FFI.
///
/// Validates email addresses according to RFC 5321 simplified rules.
/// All computation is performed by the Idris 2 verified implementation.
/// No validation logic is reimplemented in Flix.
///
mod Proven.SafeEmail {

    use Proven.LibProven.getFunction;

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
    /// Validate an email address per RFC 5321 simplified rules.
    /// Returns `Some(true)` if valid, `Some(false)` if invalid, `None` on error.
    ///
    pub def isValid(email: String): Option[Bool] \ IO =
        callBoolWithString("proven_email_is_valid", email)

    ///
    /// Validate an email address, returning the email on success.
    /// Returns `Some(email)` if valid, `None` if invalid or on error.
    ///
    pub def validate(email: String): Option[String] \ IO =
        match isValid(email) {
            case Some(true) => Some(email)
            case _          => None
        }

}
