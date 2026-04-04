// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! Aspect tests (security + boundary + FFI safety) for the proven Rust binding.
//!
//! These tests target the cross-cutting security aspects of the binding:
//!
//! - **Integer overflow prevention**: extreme values (i64::MAX, i64::MIN, 0,
//!   -1) are correctly rejected or handled by the error-mapping layer
//! - **FFI boundary safety**: null pointer, invalid argument, and allocation
//!   failure status codes are correctly classified as errors
//! - **Boundary values**: status codes at their exact boundaries (e.g., -99)
//!   map to the correct error variants
//! - **Error classification**: every error variant is distinct and not
//!   accidentally equated to a success
//! - **Thread safety**: Error and Result types are Send + Sync
//!
//! ## Note on Runtime Aspect Tests
//!
//! Runtime security tests (e.g., `SafeMath::add(i64::MAX, 1)` returning
//! `Err(Overflow)`) require `libproven.so` at runtime. These verify the full
//! overflow-prevention chain through Zig to Idris2. This suite covers the
//! Rust binding layer only.

#[cfg(test)]
mod tests {
    use crate::core::{Bounded, Error, Result, int_result_to_result, status_to_result};
    use crate::ffi::{
        IntResult, STATUS_ERR_ALLOCATION_FAILED, STATUS_ERR_DIVISION_BY_ZERO,
        STATUS_ERR_ENCODING_ERROR, STATUS_ERR_INVALID_ARGUMENT, STATUS_ERR_NOT_IMPLEMENTED,
        STATUS_ERR_NULL_POINTER, STATUS_ERR_OUT_OF_BOUNDS, STATUS_ERR_OVERFLOW,
        STATUS_ERR_PARSE_FAILURE, STATUS_ERR_UNDERFLOW, STATUS_ERR_VALIDATION_FAILED, STATUS_OK,
    };

    // =========================================================================
    // ASPECT-SEC-001: Integer overflow boundaries in Bounded<MIN, MAX>
    // =========================================================================

    /// i64::MAX is out of range for Percentage [0, 100].
    ///
    /// Security invariant: the maximum representable integer must be rejected
    /// for any type with a tighter domain. No silent truncation or wraparound.
    #[test]
    fn aspect_bounded_i64_max_is_rejected() {
        let result: Result<Bounded<0, 100>> = Bounded::new(i64::MAX);
        assert!(
            result.is_err(),
            "i64::MAX must not be accepted as a Percentage"
        );
        assert_eq!(result.unwrap_err(), Error::OutOfBounds);
    }

    /// i64::MIN is out of range for any non-negative bounded type.
    #[test]
    fn aspect_bounded_i64_min_is_rejected() {
        let result: Result<Bounded<0, 100>> = Bounded::new(i64::MIN);
        assert!(
            result.is_err(),
            "i64::MIN must not be accepted as a Percentage"
        );
        assert_eq!(result.unwrap_err(), Error::OutOfBounds);
    }

    /// -1 is out of range for zero-or-positive types.
    #[test]
    fn aspect_bounded_minus_one_is_rejected() {
        let result: Result<Bounded<0, 65535>> = Bounded::new(-1);
        assert!(
            result.is_err(),
            "-1 must not be accepted as a Port number"
        );
        assert_eq!(result.unwrap_err(), Error::OutOfBounds);
    }

    /// 65536 is out of range for Port [0, 65535].
    #[test]
    fn aspect_bounded_65536_exceeds_port_range() {
        let result: Result<Bounded<0, 65535>> = Bounded::new(65536);
        assert!(
            result.is_err(),
            "65536 must not be accepted as a Port number (max is 65535)"
        );
        assert_eq!(result.unwrap_err(), Error::OutOfBounds);
    }

    /// Boundary value: exactly 0 is valid for zero-or-positive types.
    #[test]
    fn aspect_bounded_zero_is_valid() {
        let result: Result<Bounded<0, 100>> = Bounded::new(0);
        assert!(result.is_ok(), "0 must be accepted as Percentage");
        assert_eq!(result.unwrap().get(), 0);
    }

    /// Boundary value: exactly 100 is valid for Percentage.
    #[test]
    fn aspect_bounded_100_is_valid() {
        let result: Result<Bounded<0, 100>> = Bounded::new(100);
        assert!(result.is_ok(), "100 must be accepted as Percentage");
        assert_eq!(result.unwrap().get(), 100);
    }

    /// Boundary value: exactly 101 is invalid for Percentage.
    #[test]
    fn aspect_bounded_101_exceeds_percentage() {
        let result: Result<Bounded<0, 100>> = Bounded::new(101);
        assert!(
            result.is_err(),
            "101 must not be accepted as a Percentage"
        );
        assert_eq!(result.unwrap_err(), Error::OutOfBounds);
    }

    // =========================================================================
    // ASPECT-SEC-002: FFI boundary — null pointer safety
    // =========================================================================

    /// STATUS_ERR_NULL_POINTER must never be classified as success.
    ///
    /// Security invariant: the null pointer error code must always produce
    /// an error, preventing callers from using the result of a failed FFI call.
    #[test]
    fn aspect_ffi_null_pointer_is_error() {
        let result = status_to_result(STATUS_ERR_NULL_POINTER);
        assert!(
            result.is_err(),
            "NULL_POINTER status must produce an error"
        );
        assert_eq!(result.unwrap_err(), Error::NullPointer);
    }

    /// IntResult with STATUS_ERR_NULL_POINTER must not expose the value field.
    ///
    /// A rogue FFI implementation might set value=42 alongside a null-pointer
    /// error. The binding must discard the value and return only the error.
    #[test]
    fn aspect_ffi_int_result_null_pointer_discards_value() {
        let result = int_result_to_result(IntResult {
            status: STATUS_ERR_NULL_POINTER,
            value: 42, // must be ignored
        });
        assert!(result.is_err(), "Null pointer IntResult must be Err");
        assert_eq!(result.unwrap_err(), Error::NullPointer);
    }

    // =========================================================================
    // ASPECT-SEC-003: FFI boundary — overflow/underflow not confused
    // =========================================================================

    /// Overflow and underflow must produce distinct Error variants.
    ///
    /// Security invariant: the caller must be able to distinguish between an
    /// addition overflow (result > MAX) and a subtraction underflow (< MIN).
    #[test]
    fn aspect_overflow_and_underflow_are_distinct() {
        let overflow = status_to_result(STATUS_ERR_OVERFLOW).unwrap_err();
        let underflow = status_to_result(STATUS_ERR_UNDERFLOW).unwrap_err();
        assert_ne!(
            overflow, underflow,
            "Overflow and Underflow must be distinct Error variants"
        );
    }

    /// Division by zero must not be confused with other numeric errors.
    #[test]
    fn aspect_div_zero_is_distinct_from_overflow() {
        let div_zero = status_to_result(STATUS_ERR_DIVISION_BY_ZERO).unwrap_err();
        let overflow = status_to_result(STATUS_ERR_OVERFLOW).unwrap_err();
        assert_ne!(
            div_zero, overflow,
            "DivisionByZero and Overflow must be distinct Error variants"
        );
    }

    // =========================================================================
    // ASPECT-SEC-004: FFI boundary — allocation failure safety
    // =========================================================================

    /// ALLOCATION_FAILED must be classified as an error.
    ///
    /// Security invariant: a binding that ignores allocation failures could
    /// silently write to a null/uninitialized pointer. This must never happen.
    #[test]
    fn aspect_ffi_allocation_failed_is_error() {
        let result = status_to_result(STATUS_ERR_ALLOCATION_FAILED);
        assert!(result.is_err(), "ALLOCATION_FAILED must produce an error");
        assert_eq!(result.unwrap_err(), Error::AllocationFailed);
    }

    // =========================================================================
    // ASPECT-SEC-005: Boundary status codes — extreme values
    // =========================================================================

    /// STATUS_ERR_NOT_IMPLEMENTED (-99) is a non-contiguous boundary value.
    ///
    /// Security invariant: the -99 code must be mapped to a specific Error
    /// variant rather than falling through to Error::Unknown.
    #[test]
    fn aspect_not_implemented_status_maps_correctly() {
        let result = status_to_result(STATUS_ERR_NOT_IMPLEMENTED);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), Error::NotImplemented);
    }

    /// Gaps in the status code space (-11 through -98) map to Error::Unknown.
    ///
    /// Any undocumented status code must produce a classifiable error (never
    /// silently succeed or panic).
    #[test]
    fn aspect_unknown_status_codes_produce_error() {
        for code in [-11i32, -50, -98, -100, i32::MIN, i32::MAX] {
            let result = status_to_result(code);
            assert!(
                result.is_err(),
                "Status code {} must produce an error (unknown codes must never succeed)",
                code
            );
            match result.unwrap_err() {
                Error::Unknown(c) => assert_eq!(c, code, "Unknown error must preserve the code"),
                other => panic!(
                    "Status {} produced {:?} but expected Error::Unknown",
                    code, other
                ),
            }
        }
    }

    // =========================================================================
    // ASPECT-SEC-006: IntResult — error value field is never forwarded
    // =========================================================================

    /// When the FFI returns any error status, the `value` field must never
    /// appear in the Rust result. Tests all defined error codes.
    #[test]
    fn aspect_int_result_error_value_is_discarded() {
        let error_codes = [
            STATUS_ERR_NULL_POINTER,
            STATUS_ERR_INVALID_ARGUMENT,
            STATUS_ERR_OVERFLOW,
            STATUS_ERR_UNDERFLOW,
            STATUS_ERR_DIVISION_BY_ZERO,
            STATUS_ERR_PARSE_FAILURE,
            STATUS_ERR_VALIDATION_FAILED,
            STATUS_ERR_OUT_OF_BOUNDS,
            STATUS_ERR_ENCODING_ERROR,
            STATUS_ERR_ALLOCATION_FAILED,
            STATUS_ERR_NOT_IMPLEMENTED,
        ];

        for code in error_codes {
            // Even with a "plausible" non-zero value, the result must be Err.
            let result = int_result_to_result(IntResult {
                status: code,
                value: 9999, // must be silently dropped
            });
            assert!(
                result.is_err(),
                "IntResult with error status {} must always produce Err regardless of value field",
                code
            );
        }
    }

    // =========================================================================
    // ASPECT-SEC-007: Error equality is consistent (no false equalities)
    // =========================================================================

    /// No two distinct error variants must be equal to each other.
    ///
    /// Security invariant: if error variants were accidentally equal, a caller
    /// matching on `Error::DivisionByZero` might accidentally handle
    /// `Error::Overflow` instead — bypassing the safety mechanism.
    #[test]
    fn aspect_error_variants_are_distinct() {
        let variants: Vec<Error> = vec![
            Error::NullPointer,
            Error::Overflow,
            Error::Underflow,
            Error::DivisionByZero,
            Error::OutOfBounds,
            Error::AllocationFailed,
            Error::NotImplemented,
        ];

        for (i, a) in variants.iter().enumerate() {
            for (j, b) in variants.iter().enumerate() {
                if i != j {
                    assert_ne!(
                        a, b,
                        "Error variants {:?} and {:?} must not be equal",
                        a, b
                    );
                }
            }
        }
    }

    // =========================================================================
    // ASPECT-CONC-001: Thread safety — types are Send + Sync
    // =========================================================================

    /// The Error type must be Send + Sync so it can cross thread boundaries.
    ///
    /// Safety: if a worker thread calls a proven function and the result
    /// (Err variant) needs to be sent to the main thread, Error must be Send.
    #[test]
    fn aspect_error_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Error>();
    }

    /// Result<i64> with our Error type is also Send + Sync.
    #[test]
    fn aspect_result_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Result<i64>>();
    }

    // =========================================================================
    // ASPECT-BOUNDS-001: Bounded<MIN, MAX> never exposes out-of-bounds data
    // =========================================================================

    /// A Bounded value's get() must always be within [MIN, MAX].
    ///
    /// This exercises all the "just inside" boundary values for three types.
    #[test]
    fn aspect_bounded_get_always_in_range() {
        // Percentage boundaries
        for v in [0i64, 1, 50, 99, 100] {
            let b = Bounded::<0, 100>::new(v).expect("in-range value must succeed");
            assert!(
                b.get() >= 0 && b.get() <= 100,
                "Percentage::get({}) = {} is out of range",
                v,
                b.get()
            );
        }

        // Port boundaries
        for v in [0i64, 1, 1023, 8080, 65534, 65535] {
            let b = Bounded::<0, 65535>::new(v).expect("in-range port must succeed");
            assert!(
                b.get() >= 0 && b.get() <= 65535,
                "Port::get({}) = {} is out of range",
                v,
                b.get()
            );
        }

        // Byte boundaries
        for v in [0i64, 1, 127, 254, 255] {
            let b = Bounded::<0, 255>::new(v).expect("in-range byte must succeed");
            assert!(
                b.get() >= 0 && b.get() <= 255,
                "Byte::get({}) = {} is out of range",
                v,
                b.get()
            );
        }
    }

    // =========================================================================
    // ASPECT-BOUNDS-002: STATUS_OK = 0 is the only success code
    // =========================================================================

    /// STATUS_OK (0) is the unique success code; no other value is success.
    ///
    /// This is fundamental to the FFI safety contract: positive integers,
    /// negative integers, and all non-zero values are errors.
    #[test]
    fn aspect_only_zero_is_success() {
        // Zero is success
        assert!(status_to_result(0).is_ok(), "0 must be success");

        // Adjacent values are failures
        assert!(status_to_result(1).is_err(), "1 must not be success");
        assert!(status_to_result(-1).is_err(), "-1 must not be success");
        assert!(
            status_to_result(i32::MAX).is_err(),
            "i32::MAX must not be success"
        );
        assert!(
            status_to_result(i32::MIN).is_err(),
            "i32::MIN must not be success"
        );
    }
}
