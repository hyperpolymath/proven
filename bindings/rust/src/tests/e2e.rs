// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! E2E (end-to-end / reflexive structural) tests for the proven Rust binding.
//!
//! These tests verify the full binding surface that can be exercised without
//! requiring a live `libproven.so` at runtime. They form a structural proof
//! that the binding:
//!
//! 1. Compiles with all expected modules present (build-time E2E)
//! 2. Exports the correct public symbols (status constants are correct values)
//! 3. Correctly maps the status-code chain: C ABI → IntResult → Result<i64>
//! 4. The `Error` enum covers the complete FFI error code space
//! 5. The FFI struct layout matches the expected C ABI sizes
//!
//! ## Note on Runtime E2E
//!
//! Tests that call through the Zig/Idris2 FFI (e.g., `SafeMath::add(1, 1)`)
//! require `libproven.so` at runtime and are not included here. Those tests
//! would verify the full proof chain: Rust binding → Zig FFI → Idris2 proof.
//! See `ffi/zig/test/` for the Zig-level integration tests.

#[cfg(test)]
mod tests {
    use crate::core::{Bounded, Error, NonEmpty, Result, int_result_to_result, status_to_result};
    use crate::ffi::{
        BoolResult, FloatResult, IntResult, STATUS_ERR_ALLOCATION_FAILED,
        STATUS_ERR_DIVISION_BY_ZERO, STATUS_ERR_ENCODING_ERROR, STATUS_ERR_INVALID_ARGUMENT,
        STATUS_ERR_NOT_IMPLEMENTED, STATUS_ERR_NULL_POINTER, STATUS_ERR_OUT_OF_BOUNDS,
        STATUS_ERR_OVERFLOW, STATUS_ERR_PARSE_FAILURE, STATUS_ERR_UNDERFLOW,
        STATUS_ERR_VALIDATION_FAILED, STATUS_OK,
    };

    // =========================================================================
    // E2E-STRUCT-001: FFI status code constants — complete error space
    // =========================================================================

    /// Verify that all expected status code constants are defined with correct values.
    ///
    /// This is the "error space coverage" E2E: every ProvenStatus from the
    /// Zig FFI layer must have a corresponding Rust constant with the right value.
    #[test]
    fn e2e_ffi_status_constants_are_defined() {
        assert_eq!(STATUS_OK, 0, "STATUS_OK must be 0");
        assert_eq!(STATUS_ERR_NULL_POINTER, -1);
        assert_eq!(STATUS_ERR_INVALID_ARGUMENT, -2);
        assert_eq!(STATUS_ERR_OVERFLOW, -3);
        assert_eq!(STATUS_ERR_UNDERFLOW, -4);
        assert_eq!(STATUS_ERR_DIVISION_BY_ZERO, -5);
        assert_eq!(STATUS_ERR_PARSE_FAILURE, -6);
        assert_eq!(STATUS_ERR_VALIDATION_FAILED, -7);
        assert_eq!(STATUS_ERR_OUT_OF_BOUNDS, -8);
        assert_eq!(STATUS_ERR_ENCODING_ERROR, -9);
        assert_eq!(STATUS_ERR_ALLOCATION_FAILED, -10);
        assert_eq!(STATUS_ERR_NOT_IMPLEMENTED, -99);
    }

    // =========================================================================
    // E2E-CHAIN-001: Full status-code → Error chain (proof chain simulation)
    // =========================================================================

    /// Verify the complete status→Error mapping for every defined error code.
    ///
    /// This is the closest we can get to an E2E proof-chain test without the
    /// live runtime: it exercises the full C ABI → Rust mapping layer for
    /// every error case the Zig FFI can return.
    #[test]
    fn e2e_full_status_to_error_chain() {
        let cases: &[(i32, Error)] = &[
            (STATUS_ERR_NULL_POINTER, Error::NullPointer),
            (
                STATUS_ERR_INVALID_ARGUMENT,
                Error::InvalidArgument(String::new()),
            ),
            (STATUS_ERR_OVERFLOW, Error::Overflow),
            (STATUS_ERR_UNDERFLOW, Error::Underflow),
            (STATUS_ERR_DIVISION_BY_ZERO, Error::DivisionByZero),
            (STATUS_ERR_PARSE_FAILURE, Error::ParseError(String::new())),
            (
                STATUS_ERR_VALIDATION_FAILED,
                Error::ValidationError(String::new()),
            ),
            (STATUS_ERR_OUT_OF_BOUNDS, Error::OutOfBounds),
            (
                STATUS_ERR_ENCODING_ERROR,
                Error::EncodingError(String::new()),
            ),
            (STATUS_ERR_ALLOCATION_FAILED, Error::AllocationFailed),
            (STATUS_ERR_NOT_IMPLEMENTED, Error::NotImplemented),
        ];

        for (code, expected_err) in cases {
            let result = status_to_result(*code);
            assert!(
                result.is_err(),
                "status_to_result({}) must produce Err",
                code
            );
            assert_eq!(
                result.unwrap_err(),
                *expected_err,
                "status_to_result({}) must produce {:?}",
                code,
                expected_err
            );
        }
    }

    /// IntResult chain: status=OK with a specific value must produce Ok(value).
    #[test]
    fn e2e_int_result_chain_ok() {
        let result = int_result_to_result(IntResult {
            status: STATUS_OK,
            value: 42,
        });
        assert_eq!(result, Ok(42), "OK IntResult(42) must map to Ok(42)");
    }

    /// IntResult chain: every numeric error status must produce the matching Error.
    #[test]
    fn e2e_int_result_chain_errors() {
        let overflow_result = int_result_to_result(IntResult {
            status: STATUS_ERR_OVERFLOW,
            value: 0, // value is irrelevant on error
        });
        assert_eq!(overflow_result, Err(Error::Overflow));

        let underflow_result = int_result_to_result(IntResult {
            status: STATUS_ERR_UNDERFLOW,
            value: 0,
        });
        assert_eq!(underflow_result, Err(Error::Underflow));

        let div0_result = int_result_to_result(IntResult {
            status: STATUS_ERR_DIVISION_BY_ZERO,
            value: 0,
        });
        assert_eq!(div0_result, Err(Error::DivisionByZero));
    }

    // =========================================================================
    // E2E-STRUCT-002: FFI result struct sizes (ABI compatibility check)
    // =========================================================================

    /// Verify that the FFI result structs have the expected repr(C) sizes.
    ///
    /// If the Zig FFI layer changes its struct layout, this test catches the
    /// mismatch before it causes silent memory corruption.
    #[test]
    fn e2e_ffi_struct_sizes_match_c_abi() {
        // IntResult: i32 (status) + padding + i64 (value) = 16 bytes on 64-bit
        let int_result_size = std::mem::size_of::<IntResult>();
        assert_eq!(
            int_result_size, 16,
            "IntResult must be 16 bytes (i32 + padding + i64)"
        );

        // BoolResult: i32 (status) + bool (value) = 5 bytes minimum; with alignment ≤ 8
        let bool_result_size = std::mem::size_of::<BoolResult>();
        assert!(
            bool_result_size >= 5 && bool_result_size <= 8,
            "BoolResult must be 5–8 bytes, got {}",
            bool_result_size
        );

        // FloatResult: i32 (status) + padding + f64 (value) = 16 bytes
        let float_result_size = std::mem::size_of::<FloatResult>();
        assert_eq!(
            float_result_size, 16,
            "FloatResult must be 16 bytes (i32 + padding + f64)"
        );
    }

    // =========================================================================
    // E2E-STRUCT-003: Bounded and NonEmpty are accessible from crate root
    // =========================================================================

    /// Verify type aliases are correctly exported from the crate root.
    #[test]
    fn e2e_type_aliases_exported() {
        use crate::{Bounded, NonEmpty};

        // Percentage = Bounded<0, 100>
        let pct: Result<Bounded<0, 100>> = Bounded::new(50);
        assert!(pct.is_ok(), "Percentage(50) must be valid");

        // Port = Bounded<0, 65535>
        let port: Result<Bounded<0, 65535>> = Bounded::new(8080);
        assert!(port.is_ok(), "Port(8080) must be valid");

        // Byte = Bounded<0, 255>
        let byte: Result<Bounded<0, 255>> = Bounded::new(255);
        assert!(byte.is_ok(), "Byte(255) must be valid");

        // NonEmpty is accessible
        let ne = NonEmpty::singleton(1i64);
        assert_eq!(ne.len(), 1, "NonEmpty singleton must have length 1");
    }

    // =========================================================================
    // E2E-STRUCT-004: Error trait bounds (std::error::Error + Display)
    // =========================================================================

    /// Verify Error implements std::error::Error (object-safe, boxable).
    #[test]
    fn e2e_error_implements_std_error() {
        let err: Box<dyn std::error::Error> = Box::new(Error::Overflow);
        let msg = format!("{}", err).to_lowercase();
        assert!(
            msg.contains("overflow"),
            "Error::Overflow display must mention 'overflow', got: {}",
            msg
        );
    }

    /// Verify Error::Display messages contain meaningful content.
    #[test]
    fn e2e_error_display_messages() {
        let cases: &[(Error, &str)] = &[
            (Error::NullPointer, "null"),
            (Error::Overflow, "overflow"),
            (Error::Underflow, "underflow"),
            (Error::DivisionByZero, "zero"),
            (Error::OutOfBounds, "bounds"),
            (Error::AllocationFailed, "allocation"),
            (Error::NotImplemented, "implemented"),
        ];

        for (err, expected_substr) in cases {
            let msg = format!("{}", err).to_lowercase();
            assert!(
                msg.contains(expected_substr),
                "Error::{:?} display '{}' must contain '{}'",
                err,
                msg,
                expected_substr
            );
        }
    }

    // =========================================================================
    // E2E-STRUCT-005: Bounded<MIN, MAX> bounds are accessible
    // =========================================================================

    /// Verify that the const bounds on Bounded<MIN, MAX> are correct.
    #[test]
    fn e2e_bounded_const_bounds() {
        assert_eq!(Bounded::<0, 100>::min(), 0);
        assert_eq!(Bounded::<0, 100>::max(), 100);
        assert_eq!(Bounded::<0, 65535>::min(), 0);
        assert_eq!(Bounded::<0, 65535>::max(), 65535);
        assert_eq!(Bounded::<0, 255>::min(), 0);
        assert_eq!(Bounded::<0, 255>::max(), 255);
    }

    // =========================================================================
    // E2E-STRUCT-006: IntResult alignment is correct for FFI
    // =========================================================================

    /// IntResult alignment must be 8 bytes on 64-bit platforms (C ABI match).
    #[test]
    fn e2e_int_result_alignment() {
        let align = std::mem::align_of::<IntResult>();
        assert_eq!(
            align, 8,
            "IntResult must have 8-byte alignment to match C ABI i64 field"
        );
    }
}
