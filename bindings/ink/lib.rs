// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Proven Safety Library for Ink! (Polkadot/Substrate)
//!
//! Formally verified safety primitives for Substrate smart contracts.
//! Provides safe math, bounded types, and validation for ink! contracts.
//!
//! # Example
//!
//! ```rust,ignore
//! use proven_ink::{SafeMath, Result};
//!
//! let sum = SafeMath::safe_add(100u128, 200u128)?;
//! ```
//!
//! Version: 0.9.0

#![cfg_attr(not(feature = "std"), no_std)]

use ink::prelude::string::String;
use ink::prelude::vec::Vec;

// ============================================================================
// RESULT TYPE
// ============================================================================

/// Error type for proven operations
#[derive(Debug, Clone, PartialEq, Eq, scale::Encode, scale::Decode)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
pub enum ProvenError {
    /// Arithmetic overflow
    Overflow,
    /// Arithmetic underflow
    Underflow,
    /// Division by zero
    DivisionByZero,
    /// Value out of bounds
    OutOfBounds,
    /// Invalid port number
    InvalidPort,
    /// Invalid percentage
    InvalidPercentage,
    /// Invalid address (zero address)
    InvalidAddress,
    /// Reentrancy detected
    Reentrancy,
    /// Custom error with message
    Custom(String),
}

/// Result type alias for proven operations
pub type Result<T> = core::result::Result<T, ProvenError>;

// ============================================================================
// SAFE MATH
// ============================================================================

/// Safe math operations with overflow/underflow protection
pub struct SafeMath;

impl SafeMath {
    /// Safe addition with overflow check
    #[inline]
    pub fn safe_add<T>(a: T, b: T) -> Result<T>
    where
        T: core::ops::Add<Output = T> + PartialOrd + Copy,
    {
        // Use checked_add for built-in types
        a.checked_add(&b).ok_or(ProvenError::Overflow)
    }

    /// Safe subtraction with underflow check
    #[inline]
    pub fn safe_sub<T>(a: T, b: T) -> Result<T>
    where
        T: core::ops::Sub<Output = T> + PartialOrd + Copy,
    {
        a.checked_sub(&b).ok_or(ProvenError::Underflow)
    }

    /// Safe multiplication with overflow check
    #[inline]
    pub fn safe_mul<T>(a: T, b: T) -> Result<T>
    where
        T: core::ops::Mul<Output = T> + PartialOrd + Copy,
    {
        a.checked_mul(&b).ok_or(ProvenError::Overflow)
    }

    /// Safe division with zero check
    #[inline]
    pub fn safe_div<T>(a: T, b: T) -> Result<T>
    where
        T: core::ops::Div<Output = T> + PartialEq + Default + Copy,
    {
        if b == T::default() {
            return Err(ProvenError::DivisionByZero);
        }
        Ok(a / b)
    }

    /// Safe modulo with zero check
    #[inline]
    pub fn safe_mod<T>(a: T, b: T) -> Result<T>
    where
        T: core::ops::Rem<Output = T> + PartialEq + Default + Copy,
    {
        if b == T::default() {
            return Err(ProvenError::DivisionByZero);
        }
        Ok(a % b)
    }
}

// Implement checked operations trait for standard types
trait CheckedOps: Sized {
    fn checked_add(&self, other: &Self) -> Option<Self>;
    fn checked_sub(&self, other: &Self) -> Option<Self>;
    fn checked_mul(&self, other: &Self) -> Option<Self>;
}

macro_rules! impl_checked_ops {
    ($($t:ty),*) => {
        $(
            impl CheckedOps for $t {
                #[inline]
                fn checked_add(&self, other: &Self) -> Option<Self> {
                    <$t>::checked_add(*self, *other)
                }
                #[inline]
                fn checked_sub(&self, other: &Self) -> Option<Self> {
                    <$t>::checked_sub(*self, *other)
                }
                #[inline]
                fn checked_mul(&self, other: &Self) -> Option<Self> {
                    <$t>::checked_mul(*self, *other)
                }
            }
        )*
    };
}

impl_checked_ops!(u8, u16, u32, u64, u128, i8, i16, i32, i64, i128);

// ============================================================================
// BOUNDED VALUES
// ============================================================================

/// Utilities for bounded value operations
pub struct Bounded;

impl Bounded {
    /// Clamp value to range [min, max]
    #[inline]
    pub fn clamp<T: PartialOrd>(value: T, min: T, max: T) -> T {
        if value < min {
            min
        } else if value > max {
            max
        } else {
            value
        }
    }

    /// Check if value is in range (inclusive)
    #[inline]
    pub fn in_range<T: PartialOrd>(value: T, min: T, max: T) -> bool {
        value >= min && value <= max
    }

    /// Require value in range or return error
    #[inline]
    pub fn require_in_range<T: PartialOrd + Copy>(value: T, min: T, max: T) -> Result<T> {
        if Self::in_range(value, min, max) {
            Ok(value)
        } else {
            Err(ProvenError::OutOfBounds)
        }
    }
}

/// Bounded integer type
#[derive(Debug, Clone, Copy, PartialEq, Eq, scale::Encode, scale::Decode)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
pub struct BoundedU128 {
    value: u128,
    min: u128,
    max: u128,
}

impl BoundedU128 {
    /// Create a new bounded u128
    pub fn new(value: u128, min: u128, max: u128) -> Self {
        Self {
            value: Bounded::clamp(value, min, max),
            min,
            max,
        }
    }

    /// Get the current value
    pub fn value(&self) -> u128 {
        self.value
    }

    /// Set value (clamped to bounds)
    pub fn set(&mut self, value: u128) {
        self.value = Bounded::clamp(value, self.min, self.max);
    }

    /// Safe add with clamping
    pub fn add(&mut self, delta: u128) -> Result<u128> {
        let result = SafeMath::safe_add(self.value, delta)?;
        self.value = Bounded::clamp(result, self.min, self.max);
        Ok(self.value)
    }

    /// Safe subtract with clamping
    pub fn sub(&mut self, delta: u128) -> Result<u128> {
        let result = SafeMath::safe_sub(self.value, delta)?;
        self.value = Bounded::clamp(result, self.min, self.max);
        Ok(self.value)
    }
}

// ============================================================================
// VALIDATION
// ============================================================================

/// Validation utilities
pub struct Validation;

impl Validation {
    /// Validate port number (1-65535)
    #[inline]
    pub fn is_valid_port(port: u16) -> bool {
        port >= 1
    }

    /// Require valid port or return error
    #[inline]
    pub fn require_valid_port(port: u16) -> Result<u16> {
        if Self::is_valid_port(port) {
            Ok(port)
        } else {
            Err(ProvenError::InvalidPort)
        }
    }

    /// Validate percentage (0-100)
    #[inline]
    pub fn is_valid_percentage(value: u128) -> bool {
        value <= 100
    }

    /// Require valid percentage or return error
    #[inline]
    pub fn require_valid_percentage(value: u128) -> Result<u128> {
        if Self::is_valid_percentage(value) {
            Ok(value)
        } else {
            Err(ProvenError::InvalidPercentage)
        }
    }

    /// Validate non-zero account
    #[inline]
    pub fn is_valid_account(account: &ink::primitives::AccountId) -> bool {
        // Check if account is not the zero account
        let zero = [0u8; 32];
        account.as_ref() != &zero
    }

    /// Require valid account or return error
    #[inline]
    pub fn require_valid_account(account: &ink::primitives::AccountId) -> Result<()> {
        if Self::is_valid_account(account) {
            Ok(())
        } else {
            Err(ProvenError::InvalidAddress)
        }
    }
}

// ============================================================================
// PERCENTAGE CALCULATIONS
// ============================================================================

/// Percentage calculation utilities
pub struct Percentage;

impl Percentage {
    /// Calculate percentage using basis points (100 bps = 1%)
    pub fn of_bps(amount: u128, bps: u128) -> Result<u128> {
        let product = SafeMath::safe_mul(amount, bps)?;
        SafeMath::safe_div(product, 10000)
    }

    /// Calculate percentage (0-100 scale)
    pub fn of_100(amount: u128, pct: u128) -> Result<u128> {
        if pct > 100 {
            return Err(ProvenError::InvalidPercentage);
        }
        let product = SafeMath::safe_mul(amount, pct)?;
        SafeMath::safe_div(product, 100)
    }
}

// ============================================================================
// REENTRANCY GUARD
// ============================================================================

/// Reentrancy guard state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, scale::Encode, scale::Decode)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
pub struct ReentrancyGuard {
    entered: bool,
}

impl ReentrancyGuard {
    /// Create new reentrancy guard
    pub fn new() -> Self {
        Self { entered: false }
    }

    /// Enter guarded section
    pub fn enter(&mut self) -> Result<()> {
        if self.entered {
            return Err(ProvenError::Reentrancy);
        }
        self.entered = true;
        Ok(())
    }

    /// Exit guarded section
    pub fn exit(&mut self) {
        self.entered = false;
    }

    /// Check if currently entered
    pub fn is_entered(&self) -> bool {
        self.entered
    }
}

// ============================================================================
// SAFE BALANCE
// ============================================================================

/// Safe balance type for token amounts
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, scale::Encode, scale::Decode)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
pub struct SafeBalance {
    amount: u128,
}

impl SafeBalance {
    /// Create new safe balance
    pub fn new(amount: u128) -> Self {
        Self { amount }
    }

    /// Get balance amount
    pub fn amount(&self) -> u128 {
        self.amount
    }

    /// Safe credit (add to balance)
    pub fn credit(&mut self, amount: u128) -> Result<u128> {
        self.amount = SafeMath::safe_add(self.amount, amount)?;
        Ok(self.amount)
    }

    /// Safe debit (subtract from balance)
    pub fn debit(&mut self, amount: u128) -> Result<u128> {
        self.amount = SafeMath::safe_sub(self.amount, amount)?;
        Ok(self.amount)
    }

    /// Check if can afford amount
    pub fn can_afford(&self, amount: u128) -> bool {
        self.amount >= amount
    }
}

impl From<u128> for SafeBalance {
    fn from(amount: u128) -> Self {
        Self::new(amount)
    }
}

// ============================================================================
// TIMESTAMP UTILITIES
// ============================================================================

/// Timestamp validation utilities
pub struct Timestamp;

impl Timestamp {
    /// Check if timestamp is in the future
    pub fn is_future(timestamp: u64, current: u64) -> bool {
        timestamp > current
    }

    /// Check if timestamp is in the past
    pub fn is_past(timestamp: u64, current: u64) -> bool {
        timestamp < current
    }

    /// Check if within a time window
    pub fn is_within_window(timestamp: u64, start: u64, end: u64) -> bool {
        timestamp >= start && timestamp <= end
    }

    /// Require deadline not expired
    pub fn require_not_expired(deadline: u64, current: u64) -> Result<()> {
        if current > deadline {
            Err(ProvenError::Custom(String::from("Deadline expired")))
        } else {
            Ok(())
        }
    }
}

// ============================================================================
// VERSION
// ============================================================================

/// Library version information
pub const VERSION: &str = "0.9.0";

/// Get version as tuple (major, minor, patch)
pub const fn version_tuple() -> (u8, u8, u8) {
    (0, 9, 0)
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe_add() {
        assert_eq!(SafeMath::safe_add(1u128, 2u128), Ok(3u128));
        assert_eq!(SafeMath::safe_add(0u128, 0u128), Ok(0u128));
    }

    #[test]
    fn test_safe_add_overflow() {
        let max = u128::MAX;
        assert_eq!(SafeMath::safe_add(max, 1u128), Err(ProvenError::Overflow));
    }

    #[test]
    fn test_safe_sub() {
        assert_eq!(SafeMath::safe_sub(5u128, 3u128), Ok(2u128));
        assert_eq!(SafeMath::safe_sub(100u128, 100u128), Ok(0u128));
    }

    #[test]
    fn test_safe_sub_underflow() {
        assert_eq!(SafeMath::safe_sub(1u128, 2u128), Err(ProvenError::Underflow));
    }

    #[test]
    fn test_safe_div_zero() {
        assert_eq!(
            SafeMath::safe_div(10u128, 0u128),
            Err(ProvenError::DivisionByZero)
        );
    }

    #[test]
    fn test_clamp() {
        assert_eq!(Bounded::clamp(5u128, 0u128, 10u128), 5u128);
        assert_eq!(Bounded::clamp(0u128, 5u128, 10u128), 5u128);
        assert_eq!(Bounded::clamp(15u128, 0u128, 10u128), 10u128);
    }

    #[test]
    fn test_percentage() {
        assert_eq!(Percentage::of_100(100u128, 50u128), Ok(50u128));
        assert_eq!(Percentage::of_bps(1000u128, 500u128), Ok(50u128)); // 5%
    }

    #[test]
    fn test_reentrancy_guard() {
        let mut guard = ReentrancyGuard::new();
        assert!(guard.enter().is_ok());
        assert_eq!(guard.enter(), Err(ProvenError::Reentrancy));
        guard.exit();
        assert!(guard.enter().is_ok());
    }

    #[test]
    fn test_safe_balance() {
        let mut balance = SafeBalance::new(100);
        assert_eq!(balance.credit(50), Ok(150));
        assert_eq!(balance.debit(30), Ok(120));
        assert!(balance.can_afford(100));
        assert!(!balance.can_afford(200));
    }
}
