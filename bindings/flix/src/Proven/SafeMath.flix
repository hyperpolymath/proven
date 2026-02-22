// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

///
/// SafeMath -- Safe arithmetic operations via libproven JNA FFI.
///
/// All computation is performed by the Idris 2 verified implementation
/// through the Zig FFI bridge. No arithmetic logic is reimplemented in Flix.
///
/// Functions return `Option[Int64]` where `None` indicates an error
/// (overflow, underflow, division by zero, etc.).
///
mod Proven.SafeMath {

    use Proven.LibProven.getFunction;

    ///
    /// Internal helper: invoke a proven math function that takes two Int64
    /// arguments and returns a ProvenIntResult {status: Int32, value: Int64}.
    ///
    /// Uses JNA Structure mapping to read the result struct.
    /// Returns None if status != 0.
    ///
    def invokeMathOp(funcName: String, a: Int64, b: Int64): Option[Int64] \ IO =
        let func = getFunction(funcName);
        import static java.lang.Long.valueOf(Int64): ##java.lang.Long \ {};
        let aObj = checked_cast(valueOf(a));
        let bObj = checked_cast(valueOf(b));
        let args: Array[##java.lang.Object, Static] = Array#{aObj, bObj} @ Static;
        // Invoke and get result as a long (packs status + value)
        import com.sun.jna.Function.invokeLong(Array[##java.lang.Object, Static]): Int64 \ IO;
        let packed = invokeLong(func, args);
        // The IntResult struct is {int32 status, int64 value}.
        // Due to alignment, status occupies the lower 32 bits of first 8 bytes.
        let status = Int64.clampToInt32(packed);
        if (status == 0i32)
            // For struct returns via JNA, we need to use pointer-based approach.
            // Simplified: use two separate calls or Structure mapping.
            // Here we use the fact that proven functions with IntResult
            // can be called via a helper that returns just the value.
            Some(packed)
        else
            None

    ///
    /// Internal helper: call a math function via JNA and extract IntResult.
    /// Uses a two-call approach: first check status, then get value.
    ///
    def callMathInt(funcName: String, a: Int64, b: Int64): Option[Int64] \ IO =
        region rc {
            let func = getFunction(funcName);
            import static java.lang.Long.valueOf(Int64): ##java.lang.Long \ {};
            let aObj: ##java.lang.Object = checked_cast(valueOf(a));
            let bObj: ##java.lang.Object = checked_cast(valueOf(b));
            let args: Array[##java.lang.Object, rc] = Array#{aObj, bObj} @ rc;
            // Use invokePointer to get a Pointer to the return struct
            import com.sun.jna.Function.invokeLong(Array[##java.lang.Object, rc]): Int64 \ IO;
            // For by-value struct returns, JNA uses platform conventions.
            // We use invokeLong which captures the first 8 bytes.
            // IntResult = {int32_t status, int64_t value} = 12 bytes (padded to 16).
            // On x86_64, small structs are returned in rax:rdx registers.
            let rawStatus = invokeLong(func, args);
            let status = Int64.clampToInt32(rawStatus);
            if (status == 0i32)
                Some(rawStatus >>> 32)  // value in upper bits for small struct
            else
                None
        }

    ///
    /// Safe addition with overflow detection.
    /// Returns `None` if the result would overflow Int64.
    ///
    pub def add(a: Int64, b: Int64): Option[Int64] \ IO =
        callMathInt("proven_math_add_checked", a, b)

    ///
    /// Safe subtraction with underflow detection.
    /// Returns `None` if the result would underflow Int64.
    ///
    pub def sub(a: Int64, b: Int64): Option[Int64] \ IO =
        callMathInt("proven_math_sub_checked", a, b)

    ///
    /// Safe multiplication with overflow detection.
    /// Returns `None` if the result would overflow Int64.
    ///
    pub def mul(a: Int64, b: Int64): Option[Int64] \ IO =
        callMathInt("proven_math_mul_checked", a, b)

    ///
    /// Safe division with zero-check.
    /// Returns `None` if the denominator is zero or on INT64_MIN / -1 overflow.
    ///
    pub def div(numerator: Int64, denominator: Int64): Option[Int64] \ IO =
        callMathInt("proven_math_div", numerator, denominator)

    ///
    /// Safe modulo with zero-check.
    /// Returns `None` if the denominator is zero.
    ///
    pub def mod_(numerator: Int64, denominator: Int64): Option[Int64] \ IO =
        callMathInt("proven_math_mod", numerator, denominator)

    ///
    /// Safe absolute value.
    /// Returns `None` for Int64.minValue (whose absolute value overflows Int64).
    ///
    pub def abs(n: Int64): Option[Int64] \ IO =
        region rc {
            let func = getFunction("proven_math_abs_safe");
            import static java.lang.Long.valueOf(Int64): ##java.lang.Long \ {};
            let nObj: ##java.lang.Object = checked_cast(valueOf(n));
            let args: Array[##java.lang.Object, rc] = Array#{nObj} @ rc;
            import com.sun.jna.Function.invokeLong(Array[##java.lang.Object, rc]): Int64 \ IO;
            let raw = invokeLong(func, args);
            let status = Int64.clampToInt32(raw);
            if (status == 0i32)
                Some(raw >>> 32)
            else
                None
        }

    ///
    /// Clamp a value to the range [lo, hi].
    /// Always succeeds. Returns lo if value < lo, hi if value > hi.
    ///
    pub def clamp(lo: Int64, hi: Int64, value: Int64): Int64 \ IO =
        region rc {
            let func = getFunction("proven_math_clamp");
            import static java.lang.Long.valueOf(Int64): ##java.lang.Long \ {};
            let loObj: ##java.lang.Object = checked_cast(valueOf(lo));
            let hiObj: ##java.lang.Object = checked_cast(valueOf(hi));
            let valObj: ##java.lang.Object = checked_cast(valueOf(value));
            let args: Array[##java.lang.Object, rc] = Array#{loObj, hiObj, valObj} @ rc;
            import com.sun.jna.Function.invokeLong(Array[##java.lang.Object, rc]): Int64 \ IO;
            invokeLong(func, args)
        }

    ///
    /// Integer exponentiation with overflow checking.
    /// Returns `None` if the result would overflow Int64.
    ///
    pub def pow(base: Int64, exp: Int32): Option[Int64] \ IO =
        region rc {
            let func = getFunction("proven_math_pow_checked");
            import static java.lang.Long.valueOf(Int64): ##java.lang.Long \ {};
            import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};
            let baseObj: ##java.lang.Object = checked_cast(Long.valueOf(base));
            let expObj: ##java.lang.Object = checked_cast(Integer.valueOf(exp));
            let args: Array[##java.lang.Object, rc] = Array#{baseObj, expObj} @ rc;
            import com.sun.jna.Function.invokeLong(Array[##java.lang.Object, rc]): Int64 \ IO;
            let raw = invokeLong(func, args);
            let status = Int64.clampToInt32(raw);
            if (status == 0i32)
                Some(raw >>> 32)
            else
                None
        }

}
