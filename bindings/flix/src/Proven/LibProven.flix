// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

///
/// LibProven -- Core JNA FFI declarations for libproven.
///
/// This module provides the low-level JNA (Java Native Access) bindings to
/// the libproven shared library. All computation is performed by the Idris 2
/// verified implementation via the Zig FFI bridge. No logic is reimplemented
/// in Flix.
///
/// Higher-level Safe* modules provide idiomatic Flix wrappers using
/// `Option[t]` and `Result[e, t]` for error handling.
///
mod Proven.LibProven {

    ///
    /// Status codes returned by libproven functions.
    /// Zero indicates success; negative values indicate specific error conditions.
    ///
    pub enum ProvenStatus with Eq, Order, ToString {
        case Ok
        case ErrNullPointer
        case ErrInvalidArgument
        case ErrOverflow
        case ErrUnderflow
        case ErrDivisionByZero
        case ErrParseFailure
        case ErrValidationFailed
        case ErrOutOfBounds
        case ErrEncodingError
        case ErrAllocationFailed
        case ErrNotImplemented
        case ErrUnknown(Int32)
    }

    ///
    /// Convert a raw C status code to a ProvenStatus.
    ///
    pub def statusFromInt32(code: Int32): ProvenStatus =
        match code {
            case 0i32   => ProvenStatus.Ok
            case -1i32  => ProvenStatus.ErrNullPointer
            case -2i32  => ProvenStatus.ErrInvalidArgument
            case -3i32  => ProvenStatus.ErrOverflow
            case -4i32  => ProvenStatus.ErrUnderflow
            case -5i32  => ProvenStatus.ErrDivisionByZero
            case -6i32  => ProvenStatus.ErrParseFailure
            case -7i32  => ProvenStatus.ErrValidationFailed
            case -8i32  => ProvenStatus.ErrOutOfBounds
            case -9i32  => ProvenStatus.ErrEncodingError
            case -10i32 => ProvenStatus.ErrAllocationFailed
            case -99i32 => ProvenStatus.ErrNotImplemented
            case other  => ProvenStatus.ErrUnknown(other)
        }

    ///
    /// Check if a status indicates success.
    ///
    pub def isOk(s: ProvenStatus): Bool =
        match s {
            case ProvenStatus.Ok => true
            case _               => false
        }

    ///
    /// JNA function lookup helper.
    ///
    /// Loads a function from the "proven" shared library by name.
    /// The library must be on the JNA library path (e.g., via -Djna.library.path).
    ///
    /// Returns the JNA Function object that can be invoked with appropriate arguments.
    ///
    pub def getFunction(name: String): ##com.sun.jna.Function \ IO =
        import static com.sun.jna.Function.getFunction(String, String): ##com.sun.jna.Function \ IO;
        getFunction("proven", name)

    ///
    /// Invoke a JNA function that returns Int32 with no arguments.
    ///
    pub def invokeInt32NoArgs(funcName: String): Int32 \ IO =
        let func = getFunction(funcName);
        let args: Array[##java.lang.Object, Static] = Array#{} @ Static;
        import com.sun.jna.Function.invokeInt(Array[##java.lang.Object, Static]): Int32 \ IO;
        invokeInt(func, args)

    ///
    /// Invoke a JNA function that returns Int32 with two Int64 arguments.
    /// Used for math operations that return IntResult (status is first int32 of struct).
    ///
    pub def invokeInt32WithTwoInt64(funcName: String, a: Int64, b: Int64): Int32 \ IO =
        let func = getFunction(funcName);
        import static java.lang.Long.valueOf(Int64): ##java.lang.Long \ {};
        let aObj = checked_cast(valueOf(a));
        let bObj = checked_cast(valueOf(b));
        let args: Array[##java.lang.Object, Static] = Array#{aObj, bObj} @ Static;
        import com.sun.jna.Function.invokeInt(Array[##java.lang.Object, Static]): Int32 \ IO;
        invokeInt(func, args)

    ///
    /// Initialize the proven runtime.
    /// Must be called before any other proven function.
    ///
    pub def init(): ProvenStatus \ IO =
        statusFromInt32(invokeInt32NoArgs("proven_init"))

    ///
    /// Deinitialize the proven runtime.
    ///
    pub def deinit(): Unit \ IO =
        let func = getFunction("proven_deinit");
        let args: Array[##java.lang.Object, Static] = Array#{} @ Static;
        import com.sun.jna.Function.invoke(Array[##java.lang.Object, Static]): ##java.lang.Object \ IO;
        let _ = invoke(func, args);
        ()

    ///
    /// Check if the runtime is initialized.
    ///
    pub def isInitialized(): Bool \ IO =
        invokeInt32NoArgs("proven_is_initialized") != 0i32

    ///
    /// Get the FFI ABI version number.
    ///
    pub def abiVersion(): Int32 \ IO =
        invokeInt32NoArgs("proven_ffi_abi_version")

    ///
    /// Get the library version as a tuple (major, minor, patch).
    ///
    pub def version(): (Int32, Int32, Int32) \ IO =
        let major = invokeInt32NoArgs("proven_version_major");
        let minor = invokeInt32NoArgs("proven_version_minor");
        let patch = invokeInt32NoArgs("proven_version_patch");
        (major, minor, patch)

    ///
    /// Get the total module count.
    ///
    pub def moduleCount(): Int32 \ IO =
        invokeInt32NoArgs("proven_module_count")

}
