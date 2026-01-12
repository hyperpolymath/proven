// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe arithmetic operations that cannot crash or overflow unexpectedly.
public enum SafeMath {
    /// Safely divide two integers, returning nil on division by zero.
    public static func div<T: BinaryInteger>(_ numerator: T, _ denominator: T) -> T? {
        guard denominator != 0 else { return nil }
        return numerator / denominator
    }

    /// Safely compute modulo, returning nil on division by zero.
    public static func mod<T: BinaryInteger>(_ numerator: T, _ denominator: T) -> T? {
        guard denominator != 0 else { return nil }
        return numerator % denominator
    }

    /// Safely add two integers, returning nil on overflow.
    public static func add<T: FixedWidthInteger>(_ a: T, _ b: T) -> T? {
        let (result, overflow) = a.addingReportingOverflow(b)
        return overflow ? nil : result
    }

    /// Safely subtract two integers, returning nil on overflow.
    public static func sub<T: FixedWidthInteger>(_ a: T, _ b: T) -> T? {
        let (result, overflow) = a.subtractingReportingOverflow(b)
        return overflow ? nil : result
    }

    /// Safely multiply two integers, returning nil on overflow.
    public static func mul<T: FixedWidthInteger>(_ a: T, _ b: T) -> T? {
        let (result, overflow) = a.multipliedReportingOverflow(by: b)
        return overflow ? nil : result
    }

    /// Add with overflow checking. Returns .overflow on overflow.
    public static func checkedAdd<T: FixedWidthInteger>(_ a: T, _ b: T) -> Result<T, MathError> {
        let (result, overflow) = a.addingReportingOverflow(b)
        return overflow ? .failure(.overflow) : .success(result)
    }

    /// Subtract with overflow checking. Returns .overflow on overflow.
    public static func checkedSub<T: FixedWidthInteger>(_ a: T, _ b: T) -> Result<T, MathError> {
        let (result, overflow) = a.subtractingReportingOverflow(b)
        return overflow ? .failure(.overflow) : .success(result)
    }

    /// Multiply with overflow checking. Returns .overflow on overflow.
    public static func checkedMul<T: FixedWidthInteger>(_ a: T, _ b: T) -> Result<T, MathError> {
        let (result, overflow) = a.multipliedReportingOverflow(by: b)
        return overflow ? .failure(.overflow) : .success(result)
    }

    public enum MathError: Error, Equatable {
        case overflow
        case divisionByZero
    }
}
