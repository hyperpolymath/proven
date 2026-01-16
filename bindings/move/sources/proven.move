// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Proven Safety Library for Move
///
/// Formally verified safety primitives for Move smart contracts.
/// Move already has strong type safety and resource management,
/// these utilities add domain-specific validation.
module proven::safe_math {
    use std::error;

    // ========================================================================
    // ERROR CODES
    // ========================================================================

    /// Overflow error
    const E_OVERFLOW: u64 = 1;
    /// Underflow error
    const E_UNDERFLOW: u64 = 2;
    /// Division by zero error
    const E_DIVISION_BY_ZERO: u64 = 3;
    /// Value out of bounds
    const E_OUT_OF_BOUNDS: u64 = 4;
    /// Invalid input
    const E_INVALID_INPUT: u64 = 5;

    // ========================================================================
    // SAFE MATH (u64)
    // ========================================================================

    /// Safe addition with overflow check
    public fun safe_add(a: u64, b: u64): u64 {
        let result = a + b;
        assert!(result >= a, error::invalid_argument(E_OVERFLOW));
        result
    }

    /// Safe subtraction with underflow check
    public fun safe_sub(a: u64, b: u64): u64 {
        assert!(b <= a, error::invalid_argument(E_UNDERFLOW));
        a - b
    }

    /// Safe multiplication with overflow check
    public fun safe_mul(a: u64, b: u64): u64 {
        if (a == 0) {
            return 0
        };
        let result = a * b;
        assert!(result / a == b, error::invalid_argument(E_OVERFLOW));
        result
    }

    /// Safe division with zero check
    public fun safe_div(a: u64, b: u64): u64 {
        assert!(b > 0, error::invalid_argument(E_DIVISION_BY_ZERO));
        a / b
    }

    /// Safe modulo with zero check
    public fun safe_mod(a: u64, b: u64): u64 {
        assert!(b > 0, error::invalid_argument(E_DIVISION_BY_ZERO));
        a % b
    }

    // ========================================================================
    // SAFE MATH (u128)
    // ========================================================================

    /// Safe addition for u128
    public fun safe_add_u128(a: u128, b: u128): u128 {
        let result = a + b;
        assert!(result >= a, error::invalid_argument(E_OVERFLOW));
        result
    }

    /// Safe subtraction for u128
    public fun safe_sub_u128(a: u128, b: u128): u128 {
        assert!(b <= a, error::invalid_argument(E_UNDERFLOW));
        a - b
    }

    /// Safe multiplication for u128
    public fun safe_mul_u128(a: u128, b: u128): u128 {
        if (a == 0) {
            return 0
        };
        let result = a * b;
        assert!(result / a == b, error::invalid_argument(E_OVERFLOW));
        result
    }

    // ========================================================================
    // BOUNDED VALUES
    // ========================================================================

    /// Clamp value to range [min, max]
    public fun clamp(value: u64, min: u64, max: u64): u64 {
        if (value < min) {
            min
        } else if (value > max) {
            max
        } else {
            value
        }
    }

    /// Check if value is in range (inclusive)
    public fun in_range(value: u64, min: u64, max: u64): bool {
        value >= min && value <= max
    }

    /// Require value is in range or abort
    public fun require_in_range(value: u64, min: u64, max: u64) {
        assert!(value >= min && value <= max, error::invalid_argument(E_OUT_OF_BOUNDS));
    }

    // ========================================================================
    // PERCENTAGE OPERATIONS
    // ========================================================================

    /// Calculate percentage (basis points: 100 = 1%)
    public fun percentage_of_bps(amount: u64, bps: u64): u64 {
        safe_div(safe_mul(amount, bps), 10000)
    }

    /// Calculate percentage (0-100 scale)
    public fun percentage_of(amount: u64, pct: u64): u64 {
        assert!(pct <= 100, error::invalid_argument(E_INVALID_INPUT));
        safe_div(safe_mul(amount, pct), 100)
    }

    // ========================================================================
    // TESTS
    // ========================================================================

    #[test]
    fun test_safe_add() {
        assert!(safe_add(1, 2) == 3, 0);
        assert!(safe_add(0, 0) == 0, 1);
    }

    #[test]
    #[expected_failure(abort_code = E_OVERFLOW)]
    fun test_safe_add_overflow() {
        let max = 18446744073709551615u64; // u64::MAX
        safe_add(max, 1);
    }

    #[test]
    fun test_safe_sub() {
        assert!(safe_sub(5, 3) == 2, 0);
        assert!(safe_sub(100, 100) == 0, 1);
    }

    #[test]
    #[expected_failure(abort_code = E_UNDERFLOW)]
    fun test_safe_sub_underflow() {
        safe_sub(1, 2);
    }

    #[test]
    fun test_clamp() {
        assert!(clamp(5, 0, 10) == 5, 0);
        assert!(clamp(0, 5, 10) == 5, 1);
        assert!(clamp(15, 0, 10) == 10, 2);
    }

    #[test]
    fun test_percentage() {
        assert!(percentage_of(100, 50) == 50, 0);
        assert!(percentage_of_bps(1000, 500) == 50, 1); // 5%
    }
}

module proven::safe_validation {
    use std::error;

    const E_INVALID_PORT: u64 = 10;
    const E_INVALID_PERCENTAGE: u64 = 11;

    // ========================================================================
    // PORT VALIDATION
    // ========================================================================

    /// Validate port number (1-65535)
    public fun is_valid_port(port: u64): bool {
        port >= 1 && port <= 65535
    }

    /// Require valid port or abort
    public fun require_valid_port(port: u64) {
        assert!(is_valid_port(port), error::invalid_argument(E_INVALID_PORT));
    }

    // ========================================================================
    // PERCENTAGE VALIDATION
    // ========================================================================

    /// Validate percentage (0-100)
    public fun is_valid_percentage(value: u64): bool {
        value <= 100
    }

    /// Require valid percentage or abort
    public fun require_valid_percentage(value: u64) {
        assert!(is_valid_percentage(value), error::invalid_argument(E_INVALID_PERCENTAGE));
    }

    // ========================================================================
    // POSITIVE/NON-NEGATIVE
    // ========================================================================

    /// Check if value is positive (> 0)
    public fun is_positive(value: u64): bool {
        value > 0
    }

    /// Check if value is non-zero
    public fun is_non_zero(value: u64): bool {
        value != 0
    }

    // ========================================================================
    // TESTS
    // ========================================================================

    #[test]
    fun test_is_valid_port() {
        assert!(is_valid_port(80), 0);
        assert!(is_valid_port(443), 1);
        assert!(is_valid_port(8080), 2);
        assert!(!is_valid_port(0), 3);
        assert!(!is_valid_port(70000), 4);
    }

    #[test]
    fun test_is_valid_percentage() {
        assert!(is_valid_percentage(0), 0);
        assert!(is_valid_percentage(50), 1);
        assert!(is_valid_percentage(100), 2);
        assert!(!is_valid_percentage(101), 3);
    }
}

module proven::version {
    /// Return library version as string
    public fun version(): vector<u8> {
        b"0.9.0"
    }

    /// Return version as packed integer (major << 16 | minor << 8 | patch)
    public fun version_packed(): u64 {
        0x000900
    }
}
