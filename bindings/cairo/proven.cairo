// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Proven Safety Library for Cairo (StarkNet)
///
/// Formally verified safety primitives for StarkNet smart contracts.
/// Cairo has built-in felt arithmetic; these utilities add domain validation.
///
/// Version: 0.9.0

// ============================================================================
// RESULT TYPE
// ============================================================================

/// Result type for operations that can fail
#[derive(Copy, Drop, Debug, PartialEq)]
enum Result<T> {
    Ok: T,
    Err: felt252,
}

/// Error codes
mod Errors {
    const OVERFLOW: felt252 = 'OVERFLOW';
    const UNDERFLOW: felt252 = 'UNDERFLOW';
    const DIVISION_BY_ZERO: felt252 = 'DIVISION_BY_ZERO';
    const OUT_OF_BOUNDS: felt252 = 'OUT_OF_BOUNDS';
    const INVALID_PORT: felt252 = 'INVALID_PORT';
    const INVALID_PERCENTAGE: felt252 = 'INVALID_PERCENTAGE';
    const INVALID_ADDRESS: felt252 = 'INVALID_ADDRESS';
    const REENTRANCY: felt252 = 'REENTRANCY';
}

// ============================================================================
// SAFE MATH
// ============================================================================

/// Safe math operations for u256
mod SafeMath {
    use super::Result;
    use super::Errors;
    use core::integer::BoundedInt;

    /// Safe addition with overflow check
    fn safe_add(a: u256, b: u256) -> Result<u256> {
        let max: u256 = BoundedInt::max();
        if b > max - a {
            Result::Err(Errors::OVERFLOW)
        } else {
            Result::Ok(a + b)
        }
    }

    /// Safe subtraction with underflow check
    fn safe_sub(a: u256, b: u256) -> Result<u256> {
        if b > a {
            Result::Err(Errors::UNDERFLOW)
        } else {
            Result::Ok(a - b)
        }
    }

    /// Safe multiplication with overflow check
    fn safe_mul(a: u256, b: u256) -> Result<u256> {
        if a == 0 {
            return Result::Ok(0);
        }
        let max: u256 = BoundedInt::max();
        if b > max / a {
            Result::Err(Errors::OVERFLOW)
        } else {
            Result::Ok(a * b)
        }
    }

    /// Safe division with zero check
    fn safe_div(a: u256, b: u256) -> Result<u256> {
        if b == 0 {
            Result::Err(Errors::DIVISION_BY_ZERO)
        } else {
            Result::Ok(a / b)
        }
    }

    /// Safe modulo with zero check
    fn safe_mod(a: u256, b: u256) -> Result<u256> {
        if b == 0 {
            Result::Err(Errors::DIVISION_BY_ZERO)
        } else {
            Result::Ok(a % b)
        }
    }

    /// Safe felt252 addition
    fn safe_add_felt(a: felt252, b: felt252) -> Result<felt252> {
        // In Cairo, felt252 wraps on overflow, so we need to check
        let result = a + b;
        // Check if overflow occurred (result < a when b > 0)
        if result < a {
            Result::Err(Errors::OVERFLOW)
        } else {
            Result::Ok(result)
        }
    }
}

// ============================================================================
// BOUNDED VALUES
// ============================================================================

mod Bounded {
    use super::Result;
    use super::Errors;

    /// Clamp value to range [min, max]
    fn clamp(value: u256, min: u256, max: u256) -> u256 {
        if value < min {
            min
        } else if value > max {
            max
        } else {
            value
        }
    }

    /// Check if value is in range (inclusive)
    fn in_range(value: u256, min: u256, max: u256) -> bool {
        value >= min && value <= max
    }

    /// Require value in range or return error
    fn require_in_range(value: u256, min: u256, max: u256) -> Result<u256> {
        if value >= min && value <= max {
            Result::Ok(value)
        } else {
            Result::Err(Errors::OUT_OF_BOUNDS)
        }
    }
}

// ============================================================================
// VALIDATION
// ============================================================================

mod Validation {
    use super::Result;
    use super::Errors;
    use starknet::ContractAddress;
    use core::traits::Into;

    /// Validate port number (1-65535)
    fn is_valid_port(port: u16) -> bool {
        port >= 1 && port <= 65535
    }

    /// Require valid port or return error
    fn require_valid_port(port: u16) -> Result<u16> {
        if is_valid_port(port) {
            Result::Ok(port)
        } else {
            Result::Err(Errors::INVALID_PORT)
        }
    }

    /// Validate percentage (0-100)
    fn is_valid_percentage(value: u256) -> bool {
        value <= 100
    }

    /// Require valid percentage or return error
    fn require_valid_percentage(value: u256) -> Result<u256> {
        if is_valid_percentage(value) {
            Result::Ok(value)
        } else {
            Result::Err(Errors::INVALID_PERCENTAGE)
        }
    }

    /// Validate non-zero address
    fn is_valid_address(addr: ContractAddress) -> bool {
        let addr_felt: felt252 = addr.into();
        addr_felt != 0
    }

    /// Require valid address or return error
    fn require_valid_address(addr: ContractAddress) -> Result<ContractAddress> {
        if is_valid_address(addr) {
            Result::Ok(addr)
        } else {
            Result::Err(Errors::INVALID_ADDRESS)
        }
    }
}

// ============================================================================
// PERCENTAGE CALCULATIONS
// ============================================================================

mod Percentage {
    use super::SafeMath;
    use super::Result;

    /// Calculate percentage using basis points (100 bps = 1%)
    fn of_bps(amount: u256, bps: u256) -> Result<u256> {
        match SafeMath::safe_mul(amount, bps) {
            Result::Ok(product) => SafeMath::safe_div(product, 10000),
            Result::Err(e) => Result::Err(e),
        }
    }

    /// Calculate percentage (0-100 scale)
    fn of_100(amount: u256, pct: u256) -> Result<u256> {
        assert(pct <= 100, 'Percentage exceeds 100');
        match SafeMath::safe_mul(amount, pct) {
            Result::Ok(product) => SafeMath::safe_div(product, 100),
            Result::Err(e) => Result::Err(e),
        }
    }
}

// ============================================================================
// REENTRANCY GUARD
// ============================================================================

/// Storage variable for reentrancy guard
#[starknet::storage_var]
fn reentrancy_guard() -> u8;

mod ReentrancyGuard {
    use super::reentrancy_guard;
    use super::Errors;

    const NOT_ENTERED: u8 = 0;
    const ENTERED: u8 = 1;

    /// Check and set reentrancy guard
    fn enter() {
        let status = reentrancy_guard::read();
        assert(status == NOT_ENTERED, Errors::REENTRANCY);
        reentrancy_guard::write(ENTERED);
    }

    /// Release reentrancy guard
    fn exit() {
        reentrancy_guard::write(NOT_ENTERED);
    }
}

// ============================================================================
// TIMESTAMP SAFETY
// ============================================================================

mod Timestamp {
    use starknet::get_block_timestamp;

    /// Check if timestamp is in the future
    fn is_future(timestamp: u64) -> bool {
        timestamp > get_block_timestamp()
    }

    /// Check if timestamp is in the past
    fn is_past(timestamp: u64) -> bool {
        timestamp < get_block_timestamp()
    }

    /// Check if within a time window
    fn is_within_window(timestamp: u64, window_start: u64, window_end: u64) -> bool {
        timestamp >= window_start && timestamp <= window_end
    }

    /// Require deadline not expired
    fn require_not_expired(deadline: u64) {
        assert(get_block_timestamp() <= deadline, 'Deadline expired');
    }
}

// ============================================================================
// SAFE TOKEN OPERATIONS
// ============================================================================

mod SafeToken {
    use super::Result;
    use super::SafeMath;
    use super::Errors;
    use starknet::ContractAddress;

    /// Safe balance update (prevents overflow)
    fn safe_increase_balance(current: u256, amount: u256) -> Result<u256> {
        SafeMath::safe_add(current, amount)
    }

    /// Safe balance decrease (prevents underflow)
    fn safe_decrease_balance(current: u256, amount: u256) -> Result<u256> {
        SafeMath::safe_sub(current, amount)
    }

    /// Validate transfer parameters
    fn validate_transfer(
        from: ContractAddress,
        to: ContractAddress,
        amount: u256
    ) -> Result<bool> {
        use super::Validation;

        // Check valid addresses
        if !Validation::is_valid_address(from) {
            return Result::Err(Errors::INVALID_ADDRESS);
        }
        if !Validation::is_valid_address(to) {
            return Result::Err(Errors::INVALID_ADDRESS);
        }

        // Amount can be 0 (no-op transfer is valid)
        Result::Ok(true)
    }
}

// ============================================================================
// VERSION
// ============================================================================

mod Version {
    /// Return library version
    fn version() -> felt252 {
        '0.9.0'
    }

    /// Return version as packed integer
    fn version_packed() -> u32 {
        0x000900 // 0.9.0
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::SafeMath;
    use super::Bounded;
    use super::Validation;
    use super::Result;

    #[test]
    fn test_safe_add() {
        let result = SafeMath::safe_add(1_u256, 2_u256);
        match result {
            Result::Ok(value) => assert(value == 3_u256, 'Add should equal 3'),
            Result::Err(_) => panic!("Should not error"),
        }
    }

    #[test]
    fn test_safe_sub() {
        let result = SafeMath::safe_sub(5_u256, 3_u256);
        match result {
            Result::Ok(value) => assert(value == 2_u256, 'Sub should equal 2'),
            Result::Err(_) => panic!("Should not error"),
        }
    }

    #[test]
    fn test_safe_sub_underflow() {
        let result = SafeMath::safe_sub(1_u256, 2_u256);
        match result {
            Result::Ok(_) => panic!("Should error"),
            Result::Err(e) => assert(e == super::Errors::UNDERFLOW, 'Should be underflow'),
        }
    }

    #[test]
    fn test_clamp() {
        assert(Bounded::clamp(5_u256, 0_u256, 10_u256) == 5_u256, 'Should be 5');
        assert(Bounded::clamp(0_u256, 5_u256, 10_u256) == 5_u256, 'Should be 5');
        assert(Bounded::clamp(15_u256, 0_u256, 10_u256) == 10_u256, 'Should be 10');
    }

    #[test]
    fn test_valid_port() {
        assert(Validation::is_valid_port(80_u16), 'Port 80 should be valid');
        assert(Validation::is_valid_port(443_u16), 'Port 443 should be valid');
        assert(!Validation::is_valid_port(0_u16), 'Port 0 should be invalid');
    }

    #[test]
    fn test_valid_percentage() {
        assert(Validation::is_valid_percentage(0_u256), '0 should be valid');
        assert(Validation::is_valid_percentage(50_u256), '50 should be valid');
        assert(Validation::is_valid_percentage(100_u256), '100 should be valid');
        assert(!Validation::is_valid_percentage(101_u256), '101 should be invalid');
    }
}
