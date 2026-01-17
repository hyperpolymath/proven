// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// SafeFloat prevents NaN and Infinity values.
public struct SafeFloat: Equatable, Comparable, Hashable {
    public let value: Double

    private init(_ value: Double) {
        self.value = value
    }

    /// Create a SafeFloat, returning nil if the value is NaN or infinite.
    public static func create(_ value: Double) -> SafeFloat? {
        guard value.isFinite else { return nil }
        return SafeFloat(value)
    }

    /// Create a SafeFloat with a default for invalid values.
    public static func createOrDefault(_ value: Double, default defaultValue: Double = 0) -> SafeFloat {
        if value.isFinite {
            return SafeFloat(value)
        }
        return SafeFloat(defaultValue)
    }

    public static let zero = SafeFloat(0)
    public static let one = SafeFloat(1)

    // MARK: - Arithmetic

    public func add(_ other: SafeFloat) -> SafeFloat? {
        SafeFloat.create(value + other.value)
    }

    public func subtract(_ other: SafeFloat) -> SafeFloat? {
        SafeFloat.create(value - other.value)
    }

    public func multiply(_ other: SafeFloat) -> SafeFloat? {
        SafeFloat.create(value * other.value)
    }

    public func divide(_ other: SafeFloat) -> SafeFloat? {
        guard other.value != 0 else { return nil }
        return SafeFloat.create(value / other.value)
    }

    public func power(_ exponent: SafeFloat) -> SafeFloat? {
        SafeFloat.create(pow(value, exponent.value))
    }

    public func sqrt() -> SafeFloat? {
        guard value >= 0 else { return nil }
        return SafeFloat.create(Foundation.sqrt(value))
    }

    public func abs() -> SafeFloat {
        SafeFloat(Swift.abs(value))
    }

    public func negate() -> SafeFloat {
        SafeFloat(-value)
    }

    // MARK: - Comparison

    public static func < (lhs: SafeFloat, rhs: SafeFloat) -> Bool {
        lhs.value < rhs.value
    }

    public func isZero() -> Bool {
        value == 0
    }

    public func isPositive() -> Bool {
        value > 0
    }

    public func isNegative() -> Bool {
        value < 0
    }

    // MARK: - Rounding

    public func round() -> SafeFloat {
        SafeFloat(Foundation.round(value))
    }

    public func floor() -> SafeFloat {
        SafeFloat(Foundation.floor(value))
    }

    public func ceil() -> SafeFloat {
        SafeFloat(Foundation.ceil(value))
    }

    public func truncate() -> SafeFloat {
        SafeFloat(Foundation.trunc(value))
    }

    public func round(decimals: Int) -> SafeFloat {
        let factor = pow(10.0, Double(decimals))
        return SafeFloat(Foundation.round(value * factor) / factor)
    }
}
