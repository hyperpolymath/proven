// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! P2P (property-based) tests for the proven Rust binding.
//!
//! Uses `proptest` to verify invariants that must hold across arbitrary inputs.
//! These tests target the pure-Rust portions of the binding that do not require
//! the `libproven.so` runtime: core types (`Bounded`, `NonEmpty`), error
//! conversions, and the status-code mapping contract.
//!
//! When `libproven.so` is available, the FFI-backed `SafeMath` functions can
//! be tested with the `ffi-runtime` feature. Those tests are skipped here to
//! keep the CI green without requiring the full build chain.

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use crate::core::{Bounded, Error, NonEmpty, Result, int_result_to_result, status_to_result};
    use crate::ffi::{
        IntResult, STATUS_ERR_DIVISION_BY_ZERO, STATUS_ERR_OVERFLOW, STATUS_ERR_UNDERFLOW,
        STATUS_ERR_VALIDATION_FAILED, STATUS_OK,
    };

    // =========================================================================
    // P2P-MATH-001: Bounded<MIN, MAX> — in-range values always succeed
    // =========================================================================

    proptest! {
        /// Any integer in [0, 100] should be accepted by Percentage (Bounded<0,100>).
        ///
        /// Invariant: `Bounded::<0, 100>::new(v)` is `Ok` iff `0 <= v <= 100`.
        #[test]
        fn prop_bounded_percentage_in_range(v in 0i64..=100i64) {
            let result: Result<Bounded<0, 100>> = Bounded::new(v);
            prop_assert!(
                result.is_ok(),
                "Bounded::new({}) should succeed for in-range value",
                v
            );
            let b = result.unwrap();
            prop_assert_eq!(b.get(), v, "Bounded::get() must return the stored value");
        }
    }

    proptest! {
        /// Values outside [0, 100] must be rejected with OutOfBounds.
        ///
        /// This is the core overflow-prevention invariant: values that violate
        /// the domain constraint are always rejected, never silently truncated.
        #[test]
        fn prop_bounded_percentage_out_of_range(v in prop::num::i64::ANY) {
            if v < 0 || v > 100 {
                let result: Result<Bounded<0, 100>> = Bounded::new(v);
                prop_assert!(
                    result.is_err(),
                    "Bounded::new({}) should fail for out-of-range value",
                    v
                );
                prop_assert_eq!(
                    result.unwrap_err(),
                    Error::OutOfBounds,
                    "Out-of-range must produce Error::OutOfBounds, not a different error"
                );
            }
        }
    }

    proptest! {
        /// Port numbers: Bounded<0, 65535> accepts exactly the valid port range.
        ///
        /// Invariant: a value is valid iff it fits in a u16 (0..=65535).
        #[test]
        fn prop_bounded_port_valid(v in 0i64..=65535i64) {
            let result: Result<Bounded<0, 65535>> = Bounded::new(v);
            prop_assert!(result.is_ok(), "Port {} should be valid", v);
        }
    }

    proptest! {
        /// Byte values: Bounded<0, 255> accepts exactly 0..=255.
        #[test]
        fn prop_bounded_byte_valid(v in 0i64..=255i64) {
            let result: Result<Bounded<0, 255>> = Bounded::new(v);
            prop_assert!(result.is_ok(), "Byte {} should be valid", v);
        }
    }

    // =========================================================================
    // P2P-MATH-002: NonEmpty<T> — length invariant
    // =========================================================================

    proptest! {
        /// NonEmpty::from_vec on a non-empty vec must succeed and preserve length.
        #[test]
        fn prop_non_empty_from_nonempty_vec(elems in prop::collection::vec(prop::num::i64::ANY, 1..=50)) {
            let expected_len = elems.len();
            let ne = NonEmpty::from_vec(elems);
            prop_assert!(ne.is_some(), "from_vec on non-empty input must succeed");
            let ne = ne.unwrap();
            prop_assert_eq!(
                ne.len(),
                expected_len,
                "NonEmpty::len() must equal the input length"
            );
        }
    }

    proptest! {
        /// NonEmpty::from_vec on empty vec must return None (invariant: never empty).
        #[test]
        fn prop_non_empty_from_empty_vec_is_none(_seed in prop::num::u8::ANY) {
            let ne: Option<NonEmpty<i64>> = NonEmpty::from_vec(vec![]);
            prop_assert!(ne.is_none(), "from_vec([]) must return None");
        }
    }

    proptest! {
        /// Round-trip: from_vec → to_vec preserves all elements in order.
        #[test]
        fn prop_non_empty_roundtrip(elems in prop::collection::vec(prop::num::i64::ANY, 1..=20)) {
            let original = elems.clone();
            let ne = NonEmpty::from_vec(elems).expect("non-empty input must succeed");
            let recovered = ne.to_vec();
            prop_assert_eq!(
                recovered,
                original,
                "to_vec(from_vec(xs)) must equal xs"
            );
        }
    }

    // =========================================================================
    // P2P-MATH-003: Status code → Error mapping is total and deterministic
    // =========================================================================

    proptest! {
        /// status_to_result(0) is always Ok(()).
        #[test]
        fn prop_status_ok_is_ok(_seed in prop::num::u8::ANY) {
            let r = status_to_result(STATUS_OK);
            prop_assert!(r.is_ok(), "STATUS_OK must map to Ok");
        }
    }

    proptest! {
        /// Any non-zero status code yields Err (the mapping is exhaustive).
        ///
        /// The invariant: a caller can never mistake an error for success by
        /// observing the wrong status value.
        #[test]
        fn prop_nonzero_status_is_err(code in prop::num::i32::ANY) {
            if code != STATUS_OK {
                let r = status_to_result(code);
                prop_assert!(r.is_err(), "Status {} must map to Err", code);
            }
        }
    }

    proptest! {
        /// IntResult with STATUS_OK always yields Ok(value).
        ///
        /// This directly mirrors the FFI contract: when Zig returns status=0
        /// the value field is valid and must be forwarded unchanged.
        #[test]
        fn prop_int_result_ok_preserves_value(v in prop::num::i64::ANY) {
            let result = int_result_to_result(IntResult {
                status: STATUS_OK,
                value: v,
            });
            prop_assert!(result.is_ok(), "OK IntResult must produce Ok");
            prop_assert_eq!(
                result.unwrap(),
                v,
                "OK IntResult value must be forwarded unchanged"
            );
        }
    }

    proptest! {
        /// IntResult with STATUS_ERR_OVERFLOW always yields Err::Overflow.
        ///
        /// Core invariant: the overflow error can never be silently dropped.
        #[test]
        fn prop_int_result_overflow_is_err(_v in prop::num::i64::ANY) {
            let result = int_result_to_result(IntResult {
                status: STATUS_ERR_OVERFLOW,
                value: _v,
            });
            prop_assert_eq!(
                result,
                Err(Error::Overflow),
                "STATUS_ERR_OVERFLOW must always produce Error::Overflow"
            );
        }
    }

    proptest! {
        /// IntResult with STATUS_ERR_UNDERFLOW always yields Err::Underflow.
        #[test]
        fn prop_int_result_underflow_is_err(_v in prop::num::i64::ANY) {
            let result = int_result_to_result(IntResult {
                status: STATUS_ERR_UNDERFLOW,
                value: _v,
            });
            prop_assert_eq!(
                result,
                Err(Error::Underflow),
                "STATUS_ERR_UNDERFLOW must always produce Error::Underflow"
            );
        }
    }

    proptest! {
        /// IntResult with STATUS_ERR_DIVISION_BY_ZERO always yields Err::DivisionByZero.
        ///
        /// The value field in an error result is meaningless and must not be used.
        #[test]
        fn prop_int_result_div_zero_is_err(_v in prop::num::i64::ANY) {
            let result = int_result_to_result(IntResult {
                status: STATUS_ERR_DIVISION_BY_ZERO,
                value: _v,
            });
            prop_assert_eq!(
                result,
                Err(Error::DivisionByZero),
                "STATUS_ERR_DIVISION_BY_ZERO must always produce Error::DivisionByZero"
            );
        }
    }

    // =========================================================================
    // P2P-MATH-004: Error type is total — Display never panics
    // =========================================================================

    /// Helper: produce every known Error variant for iteration.
    fn all_error_variants() -> Vec<Error> {
        vec![
            Error::NullPointer,
            Error::InvalidArgument("test".to_string()),
            Error::Overflow,
            Error::Underflow,
            Error::DivisionByZero,
            Error::ParseError("test".to_string()),
            Error::ValidationError("test".to_string()),
            Error::OutOfBounds,
            Error::EncodingError("test".to_string()),
            Error::AllocationFailed,
            Error::NotImplemented,
            Error::Unknown(-42),
        ]
    }

    proptest! {
        /// Error::Display is defined for every variant and never panics.
        ///
        /// Property: `format!("{}", e)` is non-empty for every Error variant.
        #[test]
        fn prop_error_display_never_empty(_seed in prop::num::u8::ANY) {
            for err in all_error_variants() {
                let msg = format!("{}", err);
                prop_assert!(
                    !msg.is_empty(),
                    "Error::Display for {:?} must produce a non-empty string",
                    err
                );
            }
        }
    }

    proptest! {
        /// Error clones are equal to originals (Clone/PartialEq consistency).
        #[test]
        fn prop_error_clone_equals_original(_seed in prop::num::u8::ANY) {
            for err in all_error_variants() {
                let cloned = err.clone();
                prop_assert_eq!(
                    err,
                    cloned,
                    "Cloned Error must equal original"
                );
            }
        }
    }

    // =========================================================================
    // P2P-MATH-005: status_to_result is idempotent / deterministic
    // =========================================================================

    proptest! {
        /// Calling status_to_result twice with the same code yields the same
        /// result variant (no side effects, no randomness).
        #[test]
        fn prop_status_to_result_is_deterministic(code in prop::num::i32::ANY) {
            let r1 = status_to_result(code);
            let r2 = status_to_result(code);
            // Both must agree on Ok vs Err
            prop_assert_eq!(
                r1.is_ok(),
                r2.is_ok(),
                "status_to_result({}) must be deterministic",
                code
            );
        }
    }
}
